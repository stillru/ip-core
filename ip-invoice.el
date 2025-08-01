;;; ip-invoice.el --- Generate invoices from org-mode tasks -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 IP Management System
;; Author: IP Management System
;; Version: 2.1
;; Keywords: org, invoice, time-tracking, billing
;; Package-Requires: ((emacs "27.1") (org "9.0") (mustache "0.24") (request "0.3") (deferred "0.5"))

;;; Commentary:

;; This module generates invoices from org-mode tasks for the IP management system.
;; 
;; Features:
;; - Task-based invoices using org-mode clock entries
;; - Integration with ip-core.el for client data and ip-debug.el for logging
;; - HTML invoice generation using mustache templates
;; - Serbian invoice format with NBS QR code support
;; - Draft and final invoice states with unique ID generation
;; - Automatic task aggregation and time calculations
;; - Multi-currency support with configurable exchange rates
;; - MOD-97 reference number generation for Serbian banking
;;
;; Usage:
;;   M-x ip-invoice-month             ; Generate draft for current month
;;   M-x ip-invoice-generate-interactive ; Interactive invoice generation

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
(require 'request-deferred)

;; External dependencies
(declare-function ip-debug-log "ip-debug" (level module message &rest args))
(declare-function ip-get-client-by-id "ip-core" (client-id))
(declare-function ip-get-company-info "ip-core" ())
(declare-function ip-list-client-ids "ip-core" ())

(require 'ip-core)

;; Fallback logging if ip-debug is not available
(condition-case nil
    (require 'ip-debug)
  (error
   (defun ip-debug-log (level module message &rest args)
     "Fallback logging function that uses `message'."
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
     "Fallback debug macro that uses `ip-debug-log'."
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

(defcustom ip-invoice-nbs-qr-base-url "https://nbs.rs/QRcode/api/qr/v1/generate"
  "Base URL for NBS IPS QR code generation API."
  :type 'string
  :group 'ip-invoice)

(defcustom ip-invoice-qr-size nil
  "Optional size for NBS QR code (e.g., '200x200'). Nil for default size."
  :type '(choice string (const nil))
  :group 'ip-invoice)

(defcustom ip-invoice-qr-lang "sr_RS_Latn"
  "Language for NBS QR code error messages (sr_RS_Latn, sr_RS, or en)."
  :type '(choice (const "sr_RS_Latn") (const "sr_RS") (const "en"))
  :group 'ip-invoice)

(defcustom ip-invoice-qr-fallback "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
  "Fallback base64 PNG for QR code generation failures. Nil to disable."
  :type '(choice string (const nil))
  :group 'ip-invoice)

(defcustom ip-invoice-qr-retries 3
  "Number of retry attempts for NBS QR code API calls."
  :type 'integer
  :group 'ip-invoice)

(defcustom ip-invoice-default-exchange-rate 117.85
  "Default EUR to RSD exchange rate."
  :type 'number
  :group 'ip-invoice)

(defcustom ip-invoice-default-due-days 13
  "Default number of days for invoice due date."
  :type 'integer
  :group 'ip-invoice)

(defcustom ip-invoice-max-reference-length 25
  "Maximum length for MOD-97 reference number."
  :type 'integer
  :group 'ip-invoice)

;;; Utility Functions

(defun ip-invoice--char-to-mod97-number (char)
  "Convert CHAR to number for MOD-97 calculation.
Numbers remain as-is, letters: A=10, B=11, ..., Z=35."
  (cond
   ((and (>= char ?0) (<= char ?9)) (- char ?0))
   ((and (>= char ?A) (<= char ?Z)) (- char ?A -10))
   ((and (>= char ?a) (<= char ?z)) (- char ?a -10))
   (t nil)))

(defun ip-invoice--string-to-mod97-number (str)
  "Convert string STR to number for MOD-97 calculation.
Non-alphanumeric characters are ignored."
  (let ((digits '()))
    (dolist (char (string-to-list str))
      (when-let ((digit (ip-invoice--char-to-mod97-number char)))
        (if (< digit 10)
            (push (number-to-string digit) digits)
          (let ((digit-str (number-to-string digit)))
            (push (substring digit-str 1) digits)
            (push (substring digit-str 0 1) digits)))))
    (if digits
        (string-to-number (apply #'concat (nreverse digits)))
      0)))

(defun ip-invoice--truncate-reference-base (base-string max-length)
  "Truncate BASE-STRING to fit in max reference length, accounting for checksum."
  (let ((max-base-length (- max-length 3))) ; 2 digits + 1 dash
    (if (> (length base-string) max-base-length)
        (substring base-string 0 max-base-length)
      base-string)))

(defun ip-invoice--generate-mod97-reference (base-string)
  "Generate MOD-97 reference number for BASE-STRING.
Returns string in format XX-BASE-STRING, truncated to fit max length."
  (let* ((truncated-base (ip-invoice--truncate-reference-base 
                         base-string ip-invoice-max-reference-length))
         (clean (replace-regexp-in-string "[^[:alnum:]]" "" truncated-base))
         (num (ip-invoice--string-to-mod97-number clean))
         (checksum (- 98 (% (* num 100) 97)))
         (checksum-str (format "%02d" checksum)))
    (format "%s-%s" checksum-str truncated-base)))

(defun ip-invoice--last-day-of-month (year month)
  "Return the last day of MONTH in YEAR as string YYYY-MM-DD."
  (let ((last-day (calendar-last-day-of-month month year)))
    (format "%04d-%02d-%02d" year month 
            (if (listp last-day) (cadr last-day) last-day))))

(defun ip-invoice--generate-invoice-id (name)
  "Generate a unique invoice ID with timestamp and sequential number.
DIRECTORY is the path to the folder containing invoice files (defaults to `default-directory')."
  (let* ((dir (or ip-invoice-final-dir))
         (year (format-time-string "%Y"))
         (prefix (concat name "-"))
         ;; Find all files in the directory matching the pattern INV-YYYY-*
         (existing-files (directory-files dir nil (concat "^" prefix "[0-9]+")))
         ;; Extract the highest sequential number
         (max-number (if existing-files
                         (apply #'max
                                (mapcar (lambda (file)
                                          (string-to-number
                                           (replace-regexp-in-string
                                            (concat "^" prefix "\\([0-9]+\\)") "\\1" file)))
                                        existing-files))
                       0)))
    ;; Generate the new invoice ID with the next number, padded with zeros
    (format "%s%03d" prefix (+ max-number 1))))

;(defun ip-invoice--generate-invoice-id (name)
;  "Generate a unique invoice ID with timestamp."
;  (format-time-string "name-%Y-"))

(defun ip-invoice--format-amount (amount)
  "Format AMOUNT as string with 2 decimal places."
  (format "%.2f" (if (stringp amount) (string-to-number amount) amount)))

(defun ip-invoice--safe-string-to-number (str)
  "Safely convert STR to number, returning 0.0 if invalid."
  (if (and str (string-match-p "^[0-9]+\\.?[0-9]*$" str))
      (string-to-number str)
    0.0))

;;; Clock Entry Processing

(defun ip-invoice--client-matches-p (headline client-id)
  "Return non-nil if HEADLINE matches CLIENT-ID via property or tag."
  (let ((client-prop (org-element-property :CLIENT headline))
        (tags (org-element-property :tags headline)))
    (or (string= client-prop client-id)
        (member client-id tags))))

(defun ip-invoice--process-clock-entry (clock-elem heading rate start-ts end-ts)
  "Process single CLOCK-ELEM and return entry plist if within date range."
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
        
        (list :date date
              :description (encode-coding-string heading 'utf-8)
              :hours (ip-invoice--format-amount hours-float)
              :rate (ip-invoice--format-amount rate)
              :amount (ip-invoice--format-amount amount))))))

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
        
        (let ((rate (ip-invoice--safe-string-to-number 
                    (plist-get client-data :DEFAULT_RATE))))
          (when (<= rate 0)
            (error "Invalid or missing rate for client: %s" client-id))
          
          (ip-debug-log 'info 'invoice "Using hourly rate: %.2f EUR" rate)
          
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (headline)
              (when (ip-invoice--client-matches-p headline client-id)
                (let ((heading (org-element-property :raw-value headline)))
                  (ip-debug-log 'debug 'invoice "Processing task: %s" heading)
                  
                  (org-element-map (org-element-contents headline) 'clock
                    (lambda (clock-elem)
                      (when-let ((entry (ip-invoice--process-clock-entry 
                                        clock-elem heading rate start-ts end-ts)))
                        (push entry entries)))))))))))
    
    (ip-debug-log 'info 'invoice "Found %d clock entries" (length entries))
    (sort entries (lambda (a b) (string< (plist-get a :date) (plist-get b :date))))))

;;; QR Code Generation

(defun ip-invoice--build-qr-data (company client total-rsd period poziv-na-broj)
  "Build QR code data string for NBS IPS format."
  (let* ((clean-iban (when-let ((iban (plist-get company :IBAN)))
                       (and (string-match "RS..\\([0-9]\\{18\\}\\)" iban)
                            (match-string 1 iban))))
         (amount (string-replace "." "," total-rsd))
         (svrha (format "Uplata %s" period))
         (primalac (truncate-string-to-width
                    (concat (plist-get client :NAME) "\r" (plist-get client :ADDRESS))
                    70 nil nil "...")))
    
    (format "K:PR|V:01|C:1|R:%s|N:%s|I:RSD%s|P:%s\r%s|SF:189|S:%s|RO:%s"
            (or clean-iban "000000000000000000")
            primalac
            amount
            (plist-get company :NAME)
            (plist-get company :ADDRESS)
            svrha
            poziv-na-broj)))

(defun ip-invoice--call-nbs-api (qr-data &optional size lang retries)
  "Call NBS API to generate QR code from QR-DATA string.
SIZE is the optional QR code size (e.g., '200x200'). Nil for default size.
LANG is the language for error messages (sr_RS_Latn, sr_RS, or en; default sr_RS_Latn).
RETRIES is the number of retry attempts on failure (default 3).
Returns base64 encoded image or nil on error."
  (let* ((size (or size ""))
         (lang (or lang "sr_RS_Latn"))
         (retries (or retries 3))
         (url (format "%s%s?lang=%s"
                      ip-invoice-nbs-qr-base-url
                      (if (string-empty-p size) "" (concat "/" size))
                      lang))
         (encoded-data (encode-coding-string qr-data 'utf-8 t)))
    (ip-debug-log 'info 'invoice "📡 [QR] Sending request to NBS API: %s (retries left: %d)" url retries)
    (ip-debug-log 'debug 'invoice "📝 [QR] Raw QR data: %s" qr-data)
    (ip-debug-log 'debug 'invoice "📤 [QR] Encoded data: %s" encoded-data)

    (condition-case error-info
        (let ((result
               (deferred:sync!
                (deferred:$
                 (request-deferred
                  url
                  :type "POST"
                  :headers '(("Content-Type" . "text/plain"))
                  :data encoded-data
                  :parser 'json-read
                  :success (cl-function
                            (lambda (&key data &allow-other-keys)
                              (ip-debug-log 'debug 'invoice "✅ [QR] Raw JSON response received: %S" data)
                              (if data
                                  (let* ((status-cell (assoc 's data))
                                         (status (cdr status-cell))
                                         (code (cdr (assoc 'code status)))
                                         (desc (cdr (assoc 'desc status))))
                                    (ip-debug-log 'debug 'invoice "🔢 [QR] Response code: %s, description: %s" code desc)
                                    (if (zerop code)
                                        (let ((image-data (cdr (assoc 'i data))))
                                          (if (and image-data (stringp image-data))
                                              (progn
                                                (ip-debug-log 'success 'invoice "🎉 [QR] QR code successfully generated by NBS API")
                                                image-data)
                                            (ip-debug-log 'error 'invoice "❌ [QR] Invalid or missing :i field: %s" image-data)
                                            nil))
                                      (ip-debug-log 'error 'invoice "❌ [QR] NBS API returned error: %s (code %s)" desc code)
                                      nil))
                                (ip-debug-log 'error 'invoice "❌ [QR] Response data is empty")
                                nil)))
                  :error (cl-function
                          (lambda (&key error-thrown &allow-other-keys)
                            (ip-debug-log 'error 'invoice "🔴 [QR] HTTP request failed: %s" error-thrown)
                            (if (> retries 0)
                                (progn
                                  (ip-debug-log 'warning 'invoice "Retrying NBS API call (%d attempts left)" retries)
                                  (sleep-for 1)
                                  (ip-invoice--call-nbs-api qr-data size lang (1- retries)))
                              nil))))
                 (deferred:nextc it
                   (lambda (response)
                     (if (request-response-p response)
                         (let ((data (request-response-data response)))
                           (if (and (consp data) (assoc 'i data))
                               (cdr (assoc 'i data))
                             nil))
                       response)))))))
          (ip-debug-log 'debug 'invoice "📤 [QR] API call result: %S (type: %s)" result (type-of result))
          result)
      (error
       (ip-debug-log 'error 'invoice "💥 [QR] Unexpected error during QR generation: %s" (error-message-string error-info))
       nil))))

(defun ip-invoice--generate-qr-code-data (company client total-rsd period invoice-id)
  "Generate NBS IPS QR code by calling NBS API.
Returns base64 encoded QR code image or fallback on error."
  (unless (and (plist-get company :POZIV_BASE)
               (plist-get company :IBAN)
               (plist-get company :NAME)
               (plist-get company :ADDRESS)
               (plist-get client :NAME)
               (plist-get client :ADDRESS))
    (ip-debug-log 'error 'invoice "Missing required company or client data for QR code")
    (error "Missing required company or client data for QR code"))
  
  (let* ((poziv-base (plist-get company :POZIV_BASE))
         (base-poziv (format "%s-%s-%s" poziv-base period invoice-id))
         (poziv-na-broj (ip-invoice--generate-mod97-reference base-poziv))
         (qr-data (ip-invoice--build-qr-data company client total-rsd period poziv-na-broj))
         (qr-result (ip-invoice--call-nbs-api qr-data ip-invoice-qr-size ip-invoice-qr-lang)))
    
    (ip-debug-log 'debug 'invoice "Reference number: %s" poziv-na-broj)
    (ip-debug-log 'debug 'invoice "QR data: %s" qr-data)
    (ip-debug-log 'debug 'invoice "QR result: %S (type: %s)" qr-result (type-of qr-result))
    
    (if (and qr-result (stringp qr-result) (not (string-empty-p qr-result)))
        (progn
          (ip-debug-log 'success 'invoice "✅ [QR] Using API-generated QR code")
          qr-result)
      (progn
        (ip-debug-log 'warning 'invoice "⚠️ [QR] Using fallback QR code due to invalid API response: %S (type: %s)" qr-result (type-of qr-result))
        (if ip-invoice-qr-fallback
            ip-invoice-qr-fallback
          (error "QR code generation failed and no fallback configured"))))))
        
;;; Data Processing

(defun ip-invoice--aggregate-tasks (tasks)
  "Aggregate TASKS by description, summing hours and amounts.
Returns list of aggregated task plists."
  (let ((aggregated (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let* ((desc (plist-get task :description))
             (hours (ip-invoice--safe-string-to-number (plist-get task :hours)))
             (amount (ip-invoice--safe-string-to-number (plist-get task :amount)))
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
                            :total_hours (ip-invoice--format-amount (plist-get data :total_hours))
                            :amount (ip-invoice--format-amount (plist-get data :amount)))
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

(defun ip-invoice--calculate-totals (tasks-raw tax-rate exchange-rate)
  "Calculate invoice totals from TASKS-RAW with TAX-RATE and EXCHANGE-RATE.
Returns plist with :subtotal, :tax-amount, :total-eur, :total-rsd."
  (let* ((subtotal (cl-reduce (lambda (sum task)
                               (+ sum (ip-invoice--safe-string-to-number 
                                      (plist-get task :amount))))
                             tasks-raw :initial-value 0.0))
         (tax-amount (* subtotal (/ tax-rate 100.0)))
         (total-eur (+ subtotal tax-amount))
         (total-rsd (* total-eur exchange-rate)))
    
    (list :subtotal subtotal
          :tax-amount tax-amount
          :total-eur total-eur
          :total-rsd total-rsd)))

(defun ip-invoice-generate-data (client-id start end &optional state)
  "Generate invoice data for CLIENT-ID from START to END.
STATE can be \='final or \='draft (default)."
  (ip-debug-log 'info 'invoice "Generating invoice data for %s (%s to %s)" 
              client-id start end)
  
  (let* ((client-id (string-trim client-id))
         (client (ip-get-client-by-id client-id))
         (company (ip-get-company-info)))
    
    (unless client
      (error "Unknown client ID: %s" client-id))
    (unless company
      (error "Company information not available"))
    
    (let* ((currency (or (plist-get client :CURRENCY) "EUR"))
           (tax-rate (ip-invoice--safe-string-to-number (plist-get client :TAX_RATE)))
           (invoice-id (if (eq state 'final)
                          (ip-invoice--generate-invoice-id (plist-get client :ID))
                        (format "DRAFT-%s-%s" client-id (format-time-string "%Y%m%d"))))
           (generated (format-time-string "%Y-%m-%d"))
           (due-date (format-time-string "%Y-%m-%d" 
                                       (time-add (date-to-time end) 
                                               (days-to-time ip-invoice-default-due-days))))
           (period (format-time-string "%Y-%m" (date-to-time start)))
           (poziv-base (plist-get company :POZIV_BASE))
           (base-poziv (format "%s%s" poziv-base period))
           
           ;; Get and process tasks
           (tasks-raw (ip-invoice--get-clock-entries start end client-id))
           (tasks-aggregated (ip-invoice--aggregate-tasks tasks-raw))
           
           ;; Calculate totals
           (totals (ip-invoice--calculate-totals tasks-raw tax-rate ip-invoice-default-exchange-rate))
           
           ;; Generate QR code
           (qr-code (when ip-invoice-include-payment-slip
                     (ip-invoice--generate-qr-code-data
                      company client
                      (ip-invoice--format-amount (plist-get totals :total-rsd))
                      period invoice-id))))
      
      (when (null tasks-raw)
        (ip-debug-log 'warning 'invoice "No tasks found for period %s to %s" start end))
      
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
            :subtotal (ip-invoice--format-amount (plist-get totals :subtotal))
            :tax-rate tax-rate
            :tax-amount (ip-invoice--format-amount (plist-get totals :tax-amount))
            :total (ip-invoice--format-amount (plist-get totals :total-eur))
            :total_rsd (ip-invoice--format-amount (plist-get totals :total-rsd))
            :exchange_rate (ip-invoice--format-amount ip-invoice-default-exchange-rate)
            :poziv-na-broj (ip-invoice--generate-mod97-reference base-poziv)
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
STATE can be \='final or \='draft. Returns path to generated file."
  (let* ((invoice (ip-invoice-generate-data client-id start end state))
         (output-dir (if (eq state 'final) ip-invoice-final-dir ip-invoice-draft-dir))
         (output-file (expand-file-name 
                      (format "%s.html" (plist-get invoice :invoice-id)) 
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