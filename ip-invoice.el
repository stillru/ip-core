;;; ip-invoice.el --- Generate invoices from org-mode tasks -*- lexical-binding: t; -*-

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
(require 'org-element)
(require 'mustache)
(require 'json)
(require 'request-deferred)

;; External dependencies
(declare-function ip-debug-log "ip-debug" (level module message &rest args))
(declare-function ip-get-client-by-id "ip-core" (client-id))
(declare-function ip-get-company-info "ip-core" ())
(declare-function ip-list-client-ids "ip-core" ())

(require 'ip-core)

;; Fallback logging
(unless (fboundp 'ip-debug-log)
  (defun ip-debug-log (level module message &rest args)
    (let ((formatted (apply #'format message args)))
      (message "[%s/%s] %s" (upcase (symbol-name module)) 
               (upcase (symbol-name level)) formatted))))

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

(defcustom ip-invoice-qr-retries 3
  "Number of retry attempts for NBS QR code API calls."
  :type 'integer
  :group 'ip-invoice)

(defcustom ip-invoice-qr-fallback nil
  "Fallback base64 PNG for QR code generation failures."
  :type '(choice string (const nil))
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

;;; Core Data Types and Validation

(cl-defstruct ip-invoice-data
  "Invoice data structure."
  client
  company
  start-date
  end-date
  state               ; :draft or :final
  invoice-id
  generated-date
  due-date
  currency
  period
  tasks-plain         ; Raw task list
  tasks-aggregated    ; Aggregated tasks
  subtotal
  tax-rate
  tax-amount
  total-eur
  total-rsd
  exchange-rate
  poziv-na-broj
  qr-code)

(defun ip-invoice-validate-data (data)
  "Validate invoice DATA structure."
  (unless (ip-invoice-data-p data)
    (error "Invalid invoice data structure"))
  
  (let ((required-slots '(client company start-date end-date invoice-id total-eur)))
    (dolist (slot required-slots)
      (unless (slot-value data slot)
        (error "Missing required slot: %s" slot))))
  
  (when (and (slot-value data 'tax-rate) (< (string-to-number (slot-value data 'tax-rate)) 0))
    (error "Tax rate cannot be negative"))
  
  (when (and (slot-value data 'total-eur) (< (string-to-number (slot-value data 'total-eur)) 0))
    (error "Total amount cannot be negative"))
  
  t)

;;; Utility Functions

(defun ip-invoice--safe-string-to-number (str)
  "Safely convert STR to number, returning 0.0 if invalid."
  (if (and str (stringp str) (string-match-p "^[0-9]+\\.?[0-9]*$" str))
      (string-to-number str)
    0.0))

(defun ip-invoice--format-amount (amount)
  "Format AMOUNT as string with 2 decimal places."
  (format "%.2f" (if (stringp amount) (string-to-number amount) amount)))

(defun ip-invoice--month-end (date-string)
  "Return last day of month for DATE-STRING (YYYY-MM-DD)."
  (let* ((parts (mapcar 'string-to-number (split-string date-string "-")))
         (year (nth 0 parts))
         (month (nth 1 parts))
         (last-day (calendar-last-day-of-month month year)))
    (format "%04d-%02d-%02d" year month last-day)))

(defun ip-invoice--quarter-dates (year quarter)
  "Return (start . end) dates for QUARTER in YEAR."
  (let ((quarters '((1 . ("01-01" . "03-31"))
                    (2 . ("04-01" . "06-30"))
                    (3 . ("07-01" . "09-30"))
                    (4 . ("10-01" . "12-31")))))
    (let ((dates (alist-get quarter quarters)))
      (cons (format "%04d-%s" year (car dates))
            (format "%04d-%s" year (cdr dates))))))

(defun ip-invoice--last-day-of-month (year month)
  "Return the last day of MONTH in YEAR as string YYYY-MM-DD."
  (let ((last-day (calendar-last-day-of-month month year)))
    (format "%04d-%02d-%02d" year month 
            (if (listp last-day) (cadr last-day) last-day))))

;;; MOD-97 Reference Generation

(defun ip-invoice--char-to-mod97-number (char)
  "Convert CHAR to number for MOD-97 calculation."
  (cond
   ((and (>= char ?0) (<= char ?9)) (- char ?0))
   ((and (>= char ?A) (<= char ?Z)) (- char ?A -10))
   ((and (>= char ?a) (<= char ?z)) (- char ?a -10))
   (t 0)))

(defun ip-invoice--string-to-mod97-number (str)
  "Convert string STR to number for MOD-97 calculation."
  (let ((digits '()))
    (dolist (char (string-to-list str))
      (let ((digit (ip-invoice--char-to-mod97-number char)))
        (if (< digit 10)
            (push (number-to-string digit) digits)
          (let ((digit-str (number-to-string digit)))
            (push (substring digit-str 1) digits)
            (push (substring digit-str 0 1) digits)))))
    (if digits
        (string-to-number (apply #'concat (nreverse digits)))
      0)))

(defun ip-invoice--generate-mod97-reference (base-string)
  "Generate MOD-97 reference number for BASE-STRING."
  (let* ((clean (replace-regexp-in-string "[^[:alnum:]]" "" base-string))
         (num (ip-invoice--string-to-mod97-number clean))
         (checksum (- 98 (% (* num 100) 97)))
         (checksum-str (format "%02d" checksum)))
    (format "%s-%s" checksum-str base-string)))

;;; Invoice ID Generation

(defun ip-invoice--generate-invoice-id (client-id state)
  "Generate a unique invoice ID based on CLIENT-ID and STATE."
  (let* ((prefix (if (eq state :final)
                     (concat client-id "-")
                   "DRAFT-"))
         (dir (if (eq state :final) ip-invoice-final-dir ip-invoice-draft-dir))
         (existing-files (directory-files dir nil (concat "^" prefix "[0-9]+")))
         (max-number (if existing-files
                         (apply #'max
                                (mapcar (lambda (file)
                                          (string-to-number
                                           (replace-regexp-in-string
                                            (concat "^" prefix "\\([0-9]+\\)") "\\1" file)))
                                        existing-files))
                       0)))
    (format "%s%03d" prefix (+ max-number 1))))

;;; Clock Entry Processing

(defun ip-invoice--extract-tags-from-headline (headline)
  "Extract tags from headline string when org-element returns nil.
Handles format: Headline    :tag1:tag2:tag3:"
  (let ((raw-value (org-element-property :raw-value headline)))
    ;; Look for :tag:tag:tag: pattern at the end of line
    (when (string-match "\\s-+\\(:[-[:alnum:]_@#%:]+:\\)\\s*$" raw-value)
      (let ((tags-str (match-string 1 raw-value)))
        ;; Remove leading/trailing colons and split
        (split-string (substring tags-str 1 -1) ":" t)))))

(defun ip-invoice--client-matches-p (headline client-id)
  "Return non-nil if HEADLINE matches CLIENT-ID via property or tag.
Supports tag format: CLIENT:REPOSITORY (e.g., maketv:tasks)."
  (let ((client-prop (org-element-property :CLIENT headline))
        ;; Try both org-element property and manual extraction
        (tags (or (org-element-property :tags headline)
                  (ip-invoice--extract-tags-from-headline headline))))
    
    (ip-debug-log 'debug 'invoice "Checking task: %s" (org-element-property :raw-value headline))
    (ip-debug-log 'debug 'invoice "  Client prop: %s" client-prop)
    (ip-debug-log 'debug 'invoice "  Tags from org-element: %s" (org-element-property :tags headline))
    (ip-debug-log 'debug 'invoice "  Tags extracted: %s" tags)
    
    (let ((result (or (string= client-prop client-id)
                      (cl-some (lambda (tag)
                                 (let ((parts (split-string tag ":")))
                                   (cl-member client-id parts :test #'string=)))
                               tags))))
      (when result
        (ip-debug-log 'debug 'invoice "  ✓ Matches client %s" client-id))
      result)))

(defun ip-invoice--get-task-rate (headline default-rate)
  "Get rate for task from HEADLINE property, or use DEFAULT-RATE.
Checks for :RATE: property in the headline."
  (let ((rate-prop (org-element-property :RATE headline)))
    (if (and rate-prop (stringp rate-prop) (not (string-empty-p rate-prop)))
        (progn
          (ip-debug-log 'debug 'invoice "  Using task-specific rate: %s" rate-prop)
          (ip-invoice--safe-string-to-number rate-prop))
      (progn
        (ip-debug-log 'debug 'invoice "  Using client default rate: %s" default-rate)
        default-rate))))

(defun ip-invoice--extract-clock-entries (client-id start end)
  "Extract clock entries for CLIENT-ID between START and END dates.
Returns list of plists with :date, :description, :hours, :rate, :amount."
  (let ((entries '())
        (start-ts (date-to-time start))
        (end-ts (date-to-time (concat end " 23:59:59")))
        (tasks-file-path (expand-file-name ip-tasks-file ip-org-directory)))
    
    (ip-debug-log 'info 'invoice "Extracting clock entries for %s (%s to %s)" 
                  client-id start end)
    
    (unless (file-exists-p tasks-file-path)
      (error "Tasks file not found: %s" tasks-file-path))
    
    (with-current-buffer (find-file-noselect tasks-file-path)
      (let ((client-data (ip-get-client-by-id client-id)))
        (unless client-data
          (error "Client not found: %s" client-id))
        
        (let ((default-rate (ip-invoice--safe-string-to-number 
                             (plist-get client-data :DEFAULT_RATE))))
          (when (<= default-rate 0)
            (error "Invalid or missing default rate for client: %s" client-id))
          
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (headline)
              (when (ip-invoice--client-matches-p headline client-id)
                (let ((heading (org-element-property :raw-value headline))
                      (task-rate (ip-invoice--get-task-rate headline default-rate)))
                  
                  (ip-debug-log 'debug 'invoice "Processing task: %s" heading)
                  (ip-debug-log 'debug 'invoice "  Task rate: %s (default: %s)" 
                                task-rate default-rate)
                  
                  ;; Process clock entries
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
                                 (amount (* hours-float task-rate))
                                 (date (format-time-string "%Y-%m-%d" clock-start)))
                            
                            (ip-debug-log 'debug 'invoice "  Adding entry: %s, %.2f hours, rate %s, amount %.2f EUR" 
                                          date hours-float task-rate amount)
                            
                            (push (list :date date
                                        :description (encode-coding-string heading 'utf-8)
                                        :hours hours-float
                                        :rate task-rate
                                        :amount amount)
                                  entries)))))))))))))
    
    (nreverse entries)))

;;; Task Aggregation

(defun ip-invoice--aggregate-tasks (tasks)
  "Aggregate TASKS by description and rate, summing hours and amounts.
Tasks with different rates are kept separate."
  (let ((aggregated (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let* ((desc (plist-get task :description))
             (rate (plist-get task :rate))
             (key (format "%s|%.2f" desc rate)) ; Unique key by description and rate
             (hours (plist-get task :hours))
             (amount (plist-get task :amount))
             (existing (gethash key aggregated)))
        
        (if existing
            (puthash key
                     (list :description desc
                           :rate rate
                           :hours (+ (plist-get existing :hours) hours)
                           :amount (+ (plist-get existing :amount) amount))
                     aggregated)
          (puthash key
                   (list :description desc
                         :rate rate
                         :hours hours
                         :amount amount)
                   aggregated))))
    
    (let (result)
      (maphash (lambda (_ data)
                 (push (list :description (plist-get data :description)
                             :rate (ip-invoice--format-amount (plist-get data :rate))
                             :hours (ip-invoice--format-amount (plist-get data :hours))
                             :amount (ip-invoice--format-amount (plist-get data :amount)))
                       result))
               aggregated)
      (sort result (lambda (a b) 
                     (let ((desc-a (plist-get a :description))
                           (desc-b (plist-get b :description))
                           (rate-a (string-to-number (plist-get a :rate)))
                           (rate-b (string-to-number (plist-get b :rate))))
                       (if (string= desc-a desc-b)
                           (< rate-a rate-b)
                         (string< desc-a desc-b))))))))

;;; QR Code Generation

(defun ip-invoice--generate-qr-code (company client total-rsd period invoice-id)
  "Generate NBS IPS QR code.
Returns base64 encoded PNG or nil if disabled/failed."
  (cond
   ((not ip-invoice-include-payment-slip)
    (ip-debug-log 'info 'invoice "Payment slip generation disabled")
    nil)
   
   ((not (and (plist-get company :POZIV_BASE)
              (plist-get company :IBAN)))
    (ip-debug-log 'error 'invoice "Missing required company data for QR code")
    nil)
   
   (t
    (let* ((poziv-base (plist-get company :POZIV_BASE))
           (base-poziv (format "%s-%s-%s" poziv-base period invoice-id))
           (poziv-na-broj (ip-invoice--generate-mod97-reference base-poziv))
           (qr-data (ip-invoice--build-qr-data company client total-rsd period poziv-na-broj))
           (qr-result (ip-invoice--call-nbs-api qr-data)))
      
      (if (and qr-result (stringp qr-result) (not (string-empty-p qr-result)))
          qr-result
        (ip-debug-log 'warning 'invoice "QR code generation failed")
        nil)))))

(defun ip-invoice--build-qr-data (company client total-rsd period poziv-na-broj)
  "Build QR code data string for NBS IPS format."
  (let* ((clean-iban (when-let ((iban (plist-get company :IBAN)))
                       (and (string-match "RS..\\([0-9]\\{18\\}\\)" iban)
                            (match-string 1 iban))))
         (amount (string-replace "." "," total-rsd))
         (primalac (truncate-string-to-width
                    (concat (plist-get client :NAME) "\r" (plist-get client :ADDRESS))
                    70 nil nil "...")))
    
    (format "K:PR|V:01|C:1|R:%s|N:%s|I:RSD%s|P:%s\r%s|SF:189|S:%s|RO:%s"
            (or clean-iban "000000000000000000")
            primalac
            amount
            (plist-get company :NAME)
            (plist-get company :ADDRESS)
            (format "Uplata %s" period)
            poziv-na-broj)))

(defun ip-invoice--call-nbs-api (qr-data &optional retries)
  "Call NBS API to generate QR code."
  (let* ((retries (or retries ip-invoice-qr-retries))
         (url ip-invoice-nbs-qr-base-url))
    
    (condition-case err
        (let ((response (deferred:sync!
                          (request-deferred
                           url
                           :type "POST"
                           :headers '(("Content-Type" . "text/plain"))
                           :data (encode-coding-string qr-data 'utf-8 t)
                           :parser 'json-read))))
          (if (and (consp response) (assoc 'i response))
              (cdr (assoc 'i response))
            nil))
      (error
       (if (> retries 0)
           (progn
             (ip-debug-log 'warning 'invoice "Retrying QR API call (%d attempts left)" retries)
             (sleep-for 1)
             (ip-invoice--call-nbs-api qr-data (1- retries)))
         (progn
           (ip-debug-log 'error 'invoice "QR API failed: %s" (error-message-string err))
           (when ip-invoice-qr-fallback
             (ip-debug-log 'info 'invoice "Using fallback QR code")
             ip-invoice-qr-fallback)))))))

;;; Core Invoice Generation

(defun ip-invoice-generate-data (client-id start end &optional state)
  "Generate invoice data structure for CLIENT-ID from START to END.
STATE can be :draft (default) or :final."
  (ip-debug-log 'info 'invoice "Generating invoice data for %s (%s to %s)" 
                client-id start end)
  
  (let* ((client (ip-get-client-by-id client-id))
         (company (ip-get-company-info))
         (state (or state :draft)))
    
    (unless client
      (error "Client not found: %s" client-id))
    (unless company
      (error "Company information not available"))
    
    (let* ((tax-rate (ip-invoice--safe-string-to-number (plist-get client :TAX_RATE)))
           (invoice-id (ip-invoice--generate-invoice-id client-id state))
           (generated (format-time-string "%Y-%m-%d"))
           (due-date (format-time-string "%Y-%m-%d" 
                                         (time-add (date-to-time end) 
                                                   (days-to-time ip-invoice-default-due-days))))
           (period (format-time-string "%Y-%m" (date-to-time start)))
           
           ;; Extract and process tasks
           (tasks-raw (ip-invoice--extract-clock-entries client-id start end))
           (tasks-aggregated (ip-invoice--aggregate-tasks tasks-raw))

           ;; Calculate totals
           (subtotal (cl-reduce (lambda (sum task)
                                  (+ sum (plist-get task :amount)))
                                tasks-raw :initial-value 0.0))
           (tax-amount (* subtotal (/ tax-rate 100.0)))
           (total-eur (+ subtotal tax-amount))
           (total-rsd (* total-eur ip-invoice-default-exchange-rate))
           
           ;; Generate payment references
           (poziv-base (plist-get company :POZIV_BASE))
           (base-poziv (format "%s-%s-%s" poziv-base period invoice-id))
           (poziv-na-broj (ip-invoice--generate-mod97-reference base-poziv))
           
           ;; Generate QR code
           (qr-code (ip-invoice--generate-qr-code 
                     company client 
                     (ip-invoice--format-amount total-rsd)
                     period invoice-id)))
      
      (ip-debug-log 'info 'invoice "Tasks aggregated: %d items" (length tasks-aggregated))
      (dolist (task tasks-aggregated)
        (ip-debug-log 'debug 'invoice "  - %s: rate %s, %s hours, %s EUR" 
                      (plist-get task :description)
                      (plist-get task :rate)
                      (plist-get task :hours)
                      (plist-get task :amount)))
      
      (ip-debug-log 'info 'invoice "Subtotal: %s EUR" subtotal)
      (ip-debug-log 'info 'invoice "Tax (%.0f%%): %s EUR" tax-rate tax-amount)
      (ip-debug-log 'info 'invoice "Total: %s EUR" total-eur)
      
      ;; Create structured data
      (make-ip-invoice-data
       :client client
       :company company
       :start-date start
       :end-date end
       :state state
       :invoice-id invoice-id
       :generated-date generated
       :due-date due-date
       :currency (or (plist-get client :CURRENCY) "EUR")
       :period period
       :tasks-plain tasks-raw
       :tasks-aggregated tasks-aggregated
       :subtotal (ip-invoice--format-amount subtotal)
       :tax-rate (format "%.0f" tax-rate)
       :tax-amount (ip-invoice--format-amount tax-amount)
       :total-eur (ip-invoice--format-amount total-eur)
       :total-rsd (ip-invoice--format-amount total-rsd)
       :exchange-rate (ip-invoice--format-amount ip-invoice-default-exchange-rate)
       :poziv-na-broj poziv-na-broj
       :qr-code qr-code))))

;;; HTML Generation

(defun ip-invoice--data-to-mustache (data)
  "Convert invoice DATA to Mustache-compatible format."
  (when (ip-invoice-validate-data data)
    (list
      (cons "client" (ip-invoice--plist-to-alist (slot-value data 'client)))
      (cons "company" (ip-invoice--plist-to-alist (slot-value data 'company)))
      (cons "start" (slot-value data 'start-date))
      (cons "end" (slot-value data 'end-date))
      (cons "state" (symbol-name (slot-value data 'state)))
      (cons "invoice_id" (slot-value data 'invoice-id))
      (cons "generated" (slot-value data 'generated-date))
      (cons "due_date" (slot-value data 'due-date))
      (cons "currency" (slot-value data 'currency))
      (cons "period" (slot-value data 'period))
      (cons "tasks-aggregated" 
           (mapcar #'ip-invoice--plist-to-alist (slot-value data 'tasks-aggregated)))
      (cons "subtotal" (slot-value data 'subtotal))
      (cons "tax_rate" (slot-value data 'tax-rate))
      (cons "tax_amount" (slot-value data 'tax-amount))
      (cons "total" (slot-value data 'total-eur))
      (cons "total_rsd" (slot-value data 'total-rsd))
      (cons "exchange_rate" (slot-value data 'exchange-rate))
      (cons "poziv_na_broj" (slot-value data 'poziv-na-broj))
      (cons "payment_slip" ip-invoice-include-payment-slip)
      (cons "qr_code" (slot-value data 'qr-code)))))

(defun ip-invoice--plist-to-alist (plist)
  "Convert PLIST to alist."
  (let (result)
    (while plist
      (push (cons (downcase (substring (symbol-name (car plist)) 1))
                  (cadr plist))
            result)
      (setq plist (cddr plist)))
    (nreverse result)))

(defun ip-invoice--generate-html (data output-file)
  "Generate HTML invoice from DATA and save to OUTPUT-FILE."
  (let ((template-file (expand-file-name ip-invoice-template-file)))
    (unless (file-exists-p template-file)
      (error "Template not found: %s" template-file))
    
    (let ((template (with-temp-buffer
                      (insert-file-contents template-file)
                      (buffer-string)))
          (mustache-data (ip-invoice--data-to-mustache data)))
      (ip-debug-log 'debug 'invoice "Data for generated HTML: %s" mustache-data)
      (with-temp-file output-file
        (set-buffer-file-coding-system 'utf-8)
        (insert (mustache-render template mustache-data)))
      
      (ip-debug-log 'success 'invoice "HTML generated: %s" output-file))))

;;; Public API Functions

(defun ip-invoice-generate (client-id start &optional end state)
  "Generate invoice for CLIENT-ID from START to END.
If END is nil, use last day of START's month.
STATE can be :final or :draft (default)."
  (interactive 
   (list (completing-read "Client ID: " (ip-list-client-ids))
         (read-string "Start date (YYYY-MM-DD): ")
         (let ((end-input (read-string "End date (YYYY-MM-DD, empty for month end): ")))
           (if (string-empty-p end-input) nil end-input))
         (if (y-or-n-p "Final invoice? ") :final :draft)))
  
  (let* ((start-date (if (stringp start) start (format-time-string "%Y-%m-%d" start)))
         (end-date (or end (ip-invoice--month-end start-date)))
         (invoice-data (ip-invoice-generate-data client-id start-date end-date state))
         (output-dir (if (eq state :final) ip-invoice-final-dir ip-invoice-draft-dir))
         (output-file (expand-file-name 
                       (format "%s.html" (slot-value invoice-data 'invoice-id))
                       output-dir)))
    
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    
    (ip-invoice--generate-html invoice-data output-file)
    (message "Invoice generated: %s" output-file)
    output-file))

;;; Convenience Functions

;;;###autoload
(defun ip-invoice-month-draft (client-id)
  "Generate draft invoice for CLIENT-ID for current month."
  (interactive (list (completing-read "Client ID: " (ip-list-client-ids))))
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (start (format "%04d-%02d-01" year month)))
    (ip-invoice-generate client-id start nil :draft)))

;;;###autoload
(defun ip-invoice-month-final (client-id)
  "Generate final invoice for CLIENT-ID for current month."
  (interactive (list (completing-read "Client ID: " (ip-list-client-ids))))
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (start (format "%04d-%02d-01" year month)))
    (ip-invoice-generate client-id start nil :final)))

;;;###autoload
(defun ip-invoice-last-month-draft (client-id)
  "Generate draft invoice for CLIENT-ID for last month."
  (interactive (list (completing-read "Client ID: " (ip-list-client-ids))))
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (prev-month (if (= month 1) 12 (1- month)))
         (prev-year (if (= month 1) (1- year) year))
         (start (format "%04d-%02d-01" prev-year prev-month)))
    (ip-invoice-generate client-id start nil :draft)))

;;;###autoload
(defun ip-invoice-quarter-draft (client-id quarter)
  "Generate draft invoice for CLIENT-ID for QUARTER (1-4) of current year."
  (interactive 
   (list (completing-read "Client ID: " (ip-list-client-ids))
         (string-to-number (completing-read "Quarter: " '("1" "2" "3" "4")))))
  (let* ((year (nth 5 (decode-time)))
         (dates (ip-invoice--quarter-dates year quarter)))
    (ip-invoice-generate client-id (car dates) (cdr dates) :draft)))

;;;###autoload
(defun ip-invoice-quarter-final (client-id quarter)
  "Generate final invoice for CLIENT-ID for QUARTER (1-4) of current year."
  (interactive 
   (list (completing-read "Client ID: " (ip-list-client-ids))
         (string-to-number (completing-read "Quarter: " '("1" "2" "3" "4")))))
  (let* ((year (nth 5 (decode-time)))
         (dates (ip-invoice--quarter-dates year quarter)))
    (ip-invoice-generate client-id (car dates) (cdr dates) :final)))

;;;###autoload
(defun ip-invoice-generate-interactive ()
  "Interactively generate an invoice with user prompts."
  (interactive)
  (call-interactively 'ip-invoice-generate))

;;; Template Example Update Documentation

(defun ip-invoice--template-example-update ()
  "Return example of updated template usage for per-task rates."
  (interactive)
  (message "
For per-task rates, add :RATE: property to tasks:

* Task with custom rate    :client:tasks:
  :PROPERTIES:
  :RATE:     75.00
  :END:

* Task with default rate   :client:tasks:

In aggregated view, tasks with same description but different rates 
will appear as separate items in the invoice."))

(provide 'ip-invoice)
;;; ip-invoice.el ends here