;;; ip-invoice.el --- Generate invoices from org-mode tasks -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 IP Management System
;; Author: IP Management System
;; Version: 2.0
;; Keywords: org, invoice, time-tracking, billing
;; Package-Requires: ((emacs "27.1") (org "9.0") (mustache "0.24") (request "0.3") (deferred "0.5"))

;;; Commentary:

;; This module generates invoices from org-mode tasks for the IP management system.
;; 
;; Features:
;; - Task-based invoices using org-mode clock entries
;; - Integration with ip-core.el for client data
;; - HTML invoice generation using mustache templates
;; - Serbian invoice format with NBS QR code support
;; - Draft and final invoice states
;; - Automatic task aggregation and time calculations
;; - Multi-currency support with automatic conversion

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'mustache)
(require 'subr-x)
(require 'calendar)
(require 'json)
(require 'request)
(require 'deferred)

;; External dependencies with fallbacks
(declare-function ip-debug-log "ip-debug" (level module message &rest args))
(require 'ip-core)

(condition-case nil
    (require 'ip-debug)
  (error
   (defun ip-debug-log (level module message &rest args)
     "Fallback logging function."
     (let ((formatted-msg (apply #'format message args))
           (level-str (pcase level
                        ('info "INFO")
                        ('success "SUCCESS") 
                        ('warning "WARNING")
                        ('error "ERROR")
                        (_ "DEBUG")))
           (module-str (upcase (symbol-name module))))
       (message "[%s/%s] %s" module-str level-str formatted-msg)))
   
   (defmacro ip-debug (module message &rest args)
     "Fallback debug macro."
     `(ip-debug-log 'info ,module ,message ,@args))))

;;; Customization

(defgroup ip-invoice nil
  "Invoice generation for IP management system."
  :group 'ip-core
  :prefix "ip-invoice-")

(defcustom ip-invoice-template-file "~/Documents/ORG/templates/invoice-template.html"
  "Path to the HTML invoice template file."
  :type 'file
  :group 'ip-invoice)

(defcustom ip-invoice-draft-dir (expand-file-name "invoices/draft/" ip-org-directory)
  "Directory to store draft invoices."
  :type 'directory
  :group 'ip-invoice)

(defcustom ip-invoice-final-dir (expand-file-name "invoices/final/" ip-org-directory)
  "Directory to store finalized invoices."
  :type 'directory
  :group 'ip-invoice)

(defcustom ip-invoice-include-payment-slip t
  "Whether to include Serbian payment slip with QR code."
  :type 'boolean
  :group 'ip-invoice)

(defcustom ip-invoice-nbs-qr-generate-url "https://nbs.rs/QRcode/api/qr/v1/generate?lang=sr_RS_Latn"
  "NBS IPS endpoint for QR code generation."
  :type 'string
  :group 'ip-invoice)

(defcustom ip-invoice-default-exchange-rate 117.85
  "Default EUR to RSD exchange rate."
  :type 'number
  :group 'ip-invoice)

(defcustom ip-invoice-default-due-days 13
  "Default number of days for invoice due date."
  :type 'integer
  :group 'ip-invoice)

;;; Utility Functions

(defun ip-invoice--mod97-string-to-number (str)
  "Convert string STR to number for MOD-97 calculation.
Converts A=10, B=11, ..., Z=35. Non-alphanumeric characters are ignored."
  (let ((result ""))
    (dolist (char (append str nil))
      (cond
       ((and (>= char ?0) (<= char ?9))
        (setq result (concat result (char-to-string char))))
       ((and (>= char ?A) (<= char ?Z))
        (setq result (concat result (number-to-string (- char ?A -10)))))
       ((and (>= char ?a) (<= char ?z))
        (setq result (concat result (number-to-string (- char ?a -10)))))))
    (if (string-empty-p result) 0 (string-to-number result))))

(defun ip-invoice--generate-mod97-reference (base-string)
  "Generate MOD-97 reference number for BASE-STRING.
Returns string in format XX-BASE-STRING where XX is the checksum."
  (let* ((clean (replace-regexp-in-string "[^[:alnum:]]" "" base-string))
         (num (ip-invoice--mod97-string-to-number clean))
         (checksum (- 98 (% (* num 100) 97)))
         (checksum-str (format "%02d" checksum)))
    (format "%s-%s" checksum-str base-string)))

(defun ip-invoice--last-day-of-month (year month)
  "Return the last day of MONTH in YEAR as string YYYY-MM-DD."
  (let ((last-day (calendar-last-day-of-month month year)))
    (format "%04d-%02d-%02d" year month (if (listp last-day) (cadr last-day) last-day))))

(defun ip-invoice--generate-invoice-id ()
  "Generate a unique invoice ID with timestamp."
  (format-time-string "INV-%Y%m%d-%H%M%S"))

(defun ip-invoice--client-matches-p (client-id)
  "Return non-nil if current org entry matches CLIENT-ID via property or tag."
  (or (string= (org-entry-get nil "CLIENT") client-id)
      (member client-id (org-get-tags t))))

;;; Clock Entry Processing

(defun ip-invoice--get-clock-entries (start end client-id)
  "Get clock entries for CLIENT-ID between START and END dates.
Returns list of plists with :date, :description, :hours, :rate, :amount."
  (let ((entries '())
        (start-ts (date-to-time start))
        (end-ts (date-to-time end))
        (tasks-file-path (expand-file-name ip-tasks-file ip-org-directory)))
    
    (ip-debug-log 'info 'invoice "Searching clock entries for client: %s" client-id)
    (ip-debug-log 'info 'invoice "Date range: %s to %s" start end)
    
    (unless (file-exists-p tasks-file-path)
      (error "Tasks file not found: %s" tasks-file-path))
    
    (with-current-buffer (find-file-noselect tasks-file-path)
      (let ((client-data (ip-get-client-by-id client-id)))
        (unless client-data
          (error "Client not found: %s" client-id))
        
        (let ((rate (string-to-number (or (plist-get client-data :DEFAULT_RATE) "0"))))
          (ip-debug-log 'info 'invoice "Using hourly rate: %.2f EUR" rate)
          
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (headline)
              (let* ((heading (org-element-property :raw-value headline))
                     (client-prop (org-element-property :CLIENT headline))
                     (tags (org-element-property :tags headline))
                     (matches-client (or (string= client-prop client-id)
                                       (member client-id tags))))
                
                (when matches-client
                  (ip-debug-log 'debug 'invoice "Processing task: %s" heading)
                  
                  (org-element-map (org-element-contents headline) 'clock
                    (lambda (clock-elem)
                      (when-let* ((ts (org-element-property :value clock-elem))
                                  (duration-str (org-element-property :duration clock-elem))
                                  (raw-ts (org-element-property :raw-value ts))
                                  (clock-start (org-time-string-to-time raw-ts)))
                        
                        (when (and (time-less-p start-ts clock-start)
                                   (time-less-p clock-start end-ts))
                          (let* ((parts (split-string duration-str ":"))
                                 (hours (string-to-number (car parts)))
                                 (minutes (if (> (length parts) 1) 
                                            (string-to-number (cadr parts)) 0))
                                 (hours-float (+ hours (/ minutes 60.0)))
                                 (amount (* hours-float rate))
                                 (date (format-time-string "%Y-%m-%d" clock-start)))
                            
                            (ip-debug-log 'debug 'invoice "Clock entry: %s | %.2f h | %.2f EUR" 
                                        date hours-float amount)
                            
                            (push (list :date date
                                       :description (encode-coding-string heading 'utf-8)
                                       :hours (format "%.2f" hours-float)
                                       :rate (format "%.2f" rate)
                                       :amount (format "%.2f" amount))
                                  entries))))))))))))
    
    (ip-debug-log 'info 'invoice "Found %d clock entries" (length entries))
    (sort entries (lambda (a b) (string< (plist-get a :date) (plist-get b :date))))))

;;; QR Code Generation 

(defun ip-invoice--generate-qr-code-data (company client total-rsd period invoice-id)
  "Generate NBS IPS QR code by calling NBS API.
Returns base64 encoded QR code image or fallback on error."
  (let* ((model (plist-get company :MODEL))
         (poziv-base (plist-get company :POZIV_BASE))
         (base-poziv (format "%s-%s-%s" poziv-base period invoice-id))
         (poziv-na-broj (ip-invoice--generate-mod97-reference base-poziv))
         (clean-iban (when-let ((iban (plist-get company :IBAN)))
                       (and (string-match "RS..\\([0-9]\\{18\\}\\)" iban)
                            (match-string 1 iban))))
         (amount (string-replace "." "," total-rsd))
         (svrha (format "Uplata %s" period))
         (primalac (truncate-string-to-width
                    (concat (plist-get client :NAME) "\r" (plist-get client :ADDRESS))
                    70 nil nil "..."))
         (qr-data (format "K:PR|V:01|C:1|R:%s|N:%s|I:RSD%s|P:%s\r%s|SF:189|S:%s|RO:%s"
                          (or clean-iban "000000000000000000")
                          primalac
                          amount
                          (plist-get company :NAME)
                          (plist-get company :ADDRESS)
                          svrha
                          (or poziv-na-broj "97-0000000000-DRAFT")))
         (encoded-data (encode-coding-string qr-data 'utf-8 t)))

    (ip-debug-log 'info 'invoice "Generating QR code via NBS API")
    (ip-debug-log 'debug 'invoice "QR data: %s" qr-data)

    (condition-case err
        (let ((response (deferred:sync!
                         (request-deferred ip-invoice-nbs-qr-generate-url
                                           :type "POST"
                                           :headers '(("Content-Type" . "text/plain"))
                                           :data encoded-data
                                           :parser 'json-read
                                           :success (lambda (&rest args &key data &allow-other-keys)
                                                      (ip-debug-log 'debug 'invoice "NBS API response: %S" data)
                                                      (when-let* ((status (plist-get data :s))
                                                                  (code (plist-get status :code)))
                                                        (if (eq code 0)
                                                            (plist-get data :i)
                                                          (progn
                                                            (ip-debug-log 'error 'invoice "NBS API error: %s" 
                                                                        (plist-get status :desc))
                                                            nil))))
                                           :error (lambda (&rest args &key error-thrown &allow-other-keys)
                                                    (ip-debug-log 'error 'invoice "HTTP error: %s" error-thrown)
                                                    nil)))))
          (if (stringp response)
              (progn
                (ip-debug-log 'success 'invoice "QR code generated successfully")
                response)
            (progn
              (ip-debug-log 'warning 'invoice "Using fallback QR code")
              ;; Fallback 1x1 transparent PNG
              "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")))
      (error
       (ip-debug-log 'error 'invoice "QR generation failed: %s" (error-message-string err))
       ;; Fallback QR code
       "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="))))

;;; Data Processing

(defun ip-invoice--aggregate-tasks (tasks)
  "Aggregate TASKS by description, summing hours and amounts.
Returns list of aggregated task plists."
  (let ((aggregated (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let* ((desc (plist-get task :description))
             (hours (string-to-number (plist-get task :hours)))
             (amount (string-to-number (plist-get task :amount)))
             (existing (gethash desc aggregated)))
        (if existing
            (puthash desc
                     (list :description desc
                           :total_hours (+ (plist-get existing :total_hours) hours)
                           :amount (+ (plist-get existing :amount) amount))
                     aggregated)
          (puthash desc
                   (list :description desc
                         :total_hours hours
                         :amount amount)
                   aggregated))))
    
    (let (result)
      (maphash (lambda (desc data)
                 (push (list :description desc
                            :total_hours (format "%.2f" (plist-get data :total_hours))
                            :amount (format "%.2f" (plist-get data :amount)))
                       result))
               aggregated)
      (sort result (lambda (a b) 
                     (string< (plist-get a :description) 
                             (plist-get b :description)))))))

(defun ip-invoice--convert-plist-to-mustache-data (plist)
  "Recursively convert PLIST to Mustache-compatible alist.
Handles nested plists and lists of plists."
  (let (result)
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (push (cons (downcase (substring (symbol-name key) 1))
                    (cond
                     ((and (listp value) (keywordp (car-safe value)))
                      (ip-invoice--convert-plist-to-mustache-data value))
                     ((and (listp value) (listp (car-safe value)))
                      (mapcar #'ip-invoice--convert-plist-to-mustache-data value))
                     (t value)))
              result)
        (setq plist (cddr plist))))
    (nreverse result)))

;;; Invoice Generation

(defun ip-invoice-generate-data (client-id start end &optional state)
  "Generate invoice data for CLIENT-ID from START to END.
STATE can be 'final or 'draft (default)."
  (ip-debug-log 'info 'invoice "Generating invoice data for %s (%s to %s)" client-id start end)
  
  (let* ((client-id (string-trim client-id))
         (client (ip-get-client-by-id client-id))
         (company (ip-get-company-info)))
    
    (unless client
      (error "Unknown client ID: %s" client-id))
    
    (let* ((currency (or (plist-get client :CURRENCY) "EUR"))
           (tax-rate (string-to-number (or (plist-get client :TAX_RATE) "0")))
           (invoice-id (if (eq state 'final)
                          (ip-invoice--generate-invoice-id)
                        (format "DRAFT-%s-%s" client-id (format-time-string "%Y%m%d"))))
           (generated (format-time-string "%Y-%m-%d"))
           (due-date (format-time-string "%Y-%m-%d" 
                                       (time-add (date-to-time end) 
                                               (days-to-time ip-invoice-default-due-days))))
           (exchange-rate ip-invoice-default-exchange-rate)
           (period (format-time-string "%Y-%m" (date-to-time start)))
           
           ;; Get and process tasks
           (tasks-raw (ip-invoice--get-clock-entries start end client-id))
           (tasks-aggregated (ip-invoice--aggregate-tasks tasks-raw))
           
           ;; Calculate totals
           (subtotal (cl-reduce (lambda (sum task)
                                (+ sum (string-to-number (plist-get task :amount))))
                              tasks-raw :initial-value 0.0))
           (tax-amount (* subtotal (/ tax-rate 100.0)))
           (total-amount-eur (+ subtotal tax-amount))
           (total-amount-rsd (* total-amount-eur exchange-rate))
           
           ;; Generate QR code
           (qr-code (when ip-invoice-include-payment-slip
                     (ip-invoice--generate-qr-code-data
                      company client
                      (format "%.2f" total-amount-rsd)
                      period invoice-id))))
      
      ;; Return complete invoice data
      (list :client client
            :company company
            :start start
            :end end
            :state (if (eq state 'final) "final" "draft")
            :invoice-id invoice-id
            :generated generated
            :due_date due-date
            :currency currency
            :period period
            :tasks-plain tasks-raw
            :tasks-aggregated tasks-aggregated
            :subtotal (format "%.2f" subtotal)
            :tax-rate tax-rate
            :tax-amount (format "%.2f" tax-amount)
            :total (format "%.2f" total-amount-eur)
            :total_rsd (format "%.2f" total-amount-rsd)
            :exchange_rate (format "%.2f" exchange-rate)
            :payment_slip ip-invoice-include-payment-slip
            :qr_code qr-code))))

(defun ip-invoice--generate-html (invoice output-file)
  "Generate HTML invoice from INVOICE data and save to OUTPUT-FILE."
  (ip-debug-log 'info 'invoice "Generating HTML invoice: %s" output-file)
  
  (let ((template-file (expand-file-name ip-invoice-template-file)))
    (unless (and (file-exists-p template-file) (file-readable-p template-file))
      (error "Template not found or unreadable: %s" template-file))
    
    (let* ((template (with-temp-buffer
                      (set-buffer-file-coding-system 'utf-8)
                      (insert-file-contents template-file)
                      (buffer-string)))
           (data (ip-invoice--convert-plist-to-mustache-data invoice)))
      
      (condition-case err
          (with-temp-file output-file
            (set-buffer-file-coding-system 'utf-8)
            (insert (mustache-render template data))
            (ip-debug-log 'success 'invoice "HTML invoice generated: %s" output-file))
        (error
         (ip-debug-log 'error 'invoice "Failed to generate HTML: %s" (error-message-string err))
         (error "Failed to generate HTML: %s" (error-message-string err)))))))

(defun ip-invoice-generate (client-id start end &optional state)
  "Generate an invoice for CLIENT-ID from START to END and save to file.
STATE can be 'final or 'draft. Returns path to generated file."
  (let* ((invoice (ip-invoice-generate-data client-id start end state))
         (output-dir (if (eq state 'final) ip-invoice-final-dir ip-invoice-draft-dir))
         (output-file (expand-file-name 
                      (format "%s-%s.html" client-id (plist-get invoice :invoice-id)) 
                      output-dir)))
    
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    
    (ip-invoice--generate-html invoice output-file)
    output-file))

;;; Interactive Commands

;;;###autoload
(defun ip-invoice-month (client-id)
  "Generate a draft invoice for CLIENT-ID for the current month."
  (interactive (list (completing-read "Client ID: " (ip-list-client-ids))))
  
  (let* ((today (current-time))
         (year (string-to-number (format-time-string "%Y" today)))
         (month (string-to-number (format-time-string "%m" today)))
         (start (format "%04d-%02d-01" year month))
         (end (ip-invoice--last-day-of-month year month)))
    
    (ip-debug-log 'info 'invoice "Generating current month invoice for %s (%s to %s)" 
                client-id start end)
    
    (let ((output-file (ip-invoice-generate client-id start end 'draft)))
      (message "Draft invoice generated: %s" output-file)
      (browse-url (concat "file://" output-file)))))

;;;###autoload
(defun ip-invoice-generate-interactive ()
  "Interactively generate an invoice with user prompts."
  (interactive)
  
  (let* ((client-id (completing-read "Client ID: " (ip-list-client-ids)))
         (start (read-string "Start date (YYYY-MM-DD): "))
         (end (read-string "End date (YYYY-MM-DD): "))
         (final-p (y-or-n-p "Generate final invoice? "))
         (state (if final-p 'final 'draft)))
    
    ;; Validate date format
    (unless (and (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" start)
                 (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" end))
      (user-error "Invalid date format. Use YYYY-MM-DD"))
    
    (let ((output-file (ip-invoice-generate client-id start end state)))
      (message "Invoice generated: %s" output-file)
      (browse-url (concat "file://" output-file)))))

(provide 'ip-invoice)
;;; ip-invoice.el ends here