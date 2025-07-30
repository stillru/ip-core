;;; ip-invoice.el --- Generate invoices from org-mode tasks -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;; This module generates invoices from org-mode tasks for the IP management system.
;; Supports task-based invoices using org-mode clock entries.
;; Integrates with ip-core.el for client data and ip-debug.el for logging.
;; Outputs HTML invoices using mustache.el templates.
;; Enhanced with Serbian invoice format support.
;;; Code:

(eval-and-compile
  ;; Fallback logging if ip-debug is not available
  (unless (fboundp 'ip-debug-log)
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
        (message "[%s/%s] %s" module-str level-str formatted-msg))))
  (unless (fboundp 'ip-debug)
    (defmacro ip-debug (module message &rest args)
      "Fallback debug macro that uses `ip-debug-log'."
      `(ip-debug-log 'info ,module ,message ,@args))))

(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'ip-core)
(require 'mustache)
(require 'subr-x)
(require 'calendar)

(condition-case nil
    (require 'ip-debug)
  (error
   (ip-debug-log 'warning 'invoice "Failed to load ip-debug.el, using fallback logging")))

;;; Customization
(defgroup ip-invoice nil
  "Invoice generation for IP management system."
  :group 'ip-core)

(defcustom ip-invoice-template-file "~/Documents/ORG/templates/invoice-template.html"
  "Path to the enhanced HTML invoice template."
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
  "Include Serbian payment slip with QR code."
  :type 'boolean
  :group 'ip-invoice)

;;; Core Functions

(defun ip-invoice--get-clock-entries (start end client-id)
  "Get clock entries for CLIENT-ID between START and END."
  (let ((entries '())
        (start-ts (date-to-time start))
        (end-ts (date-to-time end)))
    (org-map-entries
     (lambda ()
       (let ((heading (org-get-heading t t))
             (tags (org-get-tags)))
         (ip-debug-log 'debug 'invoice "Scanning: %s | Tags: %S" heading tags)
         (when (member client-id tags)
           (ip-debug-log 'debug 'invoice "✅ Matched client: %s" client-id)
           (let ((desc heading)
                 (rate (string-to-number
                        (or (plist-get (ip-get-client-by-id client-id) :DEFAULT_RATE) "0"))))
             (org-element-map (org-element-at-point) 'clock
               (lambda (cl)
                 (when-let ((ts (org-element-property :value cl))
                            (duration-str (org-element-property :duration cl)))
                   (let* ((raw-ts (org-element-property :raw-value ts))
                          (clock-start (org-time-string-to-time raw-ts)))
                     (when (and (time-less-p start-ts clock-start)
                                (time-less-p clock-start end-ts))
                       (let* ((parts (split-string duration-str ":"))
                              (hours (string-to-number (car parts)))
                              (minutes (if (> (length parts) 1) (string-to-number (cadr parts)) 0))
                              (hours-float (+ hours (/ minutes 60.0)))
                              (amount (* hours-float rate))
                              (date (format-time-string "%Y-%m-%d" clock-start)))
                         (ip-debug-log 'debug 'invoice "⏱️  Clock entry: %s | %s | %.2f hours" date desc hours-float)
                         (push (list :date date
                                     :description (encode-coding-string desc 'utf-8)
                                     :hours (format "%.2f" hours-float)
                                     :rate (format "%.2f" rate)
                                     :amount (format "%.2f" amount))
                               entries)))))))))))
     t 'file)
    (sort entries (lambda (a b) (string< (plist-get a :date) (plist-get b :date))))))

(defun ip-invoice--generate-qr-code (invoice)
  "Generate NBS IPS QR code (mocked for now)."
  (let* ((company (plist-get invoice :company))
         (total-rsd (plist-get invoice :total_rsd))
         (poziv (format "%s-%s-%s"
                        (plist-get company :poziv_base)
                        (plist-get invoice :period)
                        (plist-get invoice :invoice_id)))
         (qr-data (format "K:PR|V:01|C:1|R:%s|N:%s|I:RSD%s|P:%s|SF:189|S:Račun %s za %s|RO:%s"
                          (plist-get company :iban)
                          (concat (plist-get company :name) "\r\n" (plist-get company :address))
                          total-rsd
                          (concat (plist-get invoice :client :name) "\r\n" (plist-get invoice :client :address))
                          (plist-get invoice :invoice_id)
                          (plist-get invoice :period)
                          poziv)))
    (ip-debug-log 'info 'invoice "Mock QR code for: %s" qr-data)
    "iVBORw0KGgoAAAANSUhEUgAAAMgAAADIAQAAAACFI9sAAAAH0lEQVR42u3BAQ0AAADCoPdPbQ43oAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADwDcaiAAFXqzYQAAAAAElFTkSuQmCC"))

(defun ip-invoice--generate-invoice-id ()
  "Generate a unique invoice ID."
  (format-time-string "INV-%Y%m%d-%H%M%S"))

(defun ip-invoice-generate-data (client-id start end &optional state)
  "Generate invoice data for CLIENT-ID from START to END.
Returns a plist with :tasks-plain and :tasks-aggregated."
  (ip-debug-log 'info 'invoice "Generating invoice data for %s (%s to %s)" client-id start end)
  (let* ((client-id (string-trim client-id))
         (client (or (ip-get-client-by-id client-id)
                     (progn
                       (ip-debug-log 'error 'invoice "Unknown client ID: %s" client-id)
                       (error "Unknown client ID: %s" client-id))))
         (client-name (or (plist-get client :NAME) client-id))
         (currency (or (plist-get client :CURRENCY) "EUR"))
         (tax-rate (or (string-to-number (or (plist-get client :TAX_RATE) "0")) 0.0))
         (invoice-id (if (eq state 'final)
                         (ip-invoice--generate-invoice-id)
                       (format "DRAFT-%s-%s" client-id (format-time-string "%Y%m%d"))))
         (generated (format-time-string "%Y-%m-%d"))
         (due-date (format-time-string "%Y-%m-%d" (time-add (date-to-time end) (days-to-time 13))))
         (company (ip-get-company-info))
         (exchange-rate 117.85) ; stub
         (tasks-plain ())
         (tasks-aggregated (make-hash-table :test 'equal))
         (subtotal 0.0))
    ;; Load and process entries
    (let ((entries (ip-invoice--get-clock-entries start end client-id)))
      (dolist (entry entries)
        (let* ((desc (plist-get entry :description))
               (hours (string-to-number (plist-get entry :hours)))
               (rate (string-to-number (plist-get entry :rate)))
               (amount (* hours rate)))
          ;; Add to plain
          (push entry tasks-plain)
          ;; Aggregate
          (let ((agg (gethash desc tasks-aggregated)))
            (if agg
                (puthash desc
                         (list :description desc
                               :total_hours (+ (plist-get agg :total_hours) hours)
                               :amount (+ (plist-get agg :amount) amount))
                         tasks-aggregated)
              (puthash desc
                       (list :description desc
                             :total_hours hours
                             :amount amount)
                       tasks-aggregated)))
          (setq subtotal (+ subtotal amount)))))
    ;; Convert hash-table to list
    (let ((aggregated-list (let (result)
                             (maphash (lambda (desc data)
                                        (push (list :description desc
                                                    :total_hours (format "%.2f" (plist-get data :total_hours))
                                                    :amount (format "%.2f" (plist-get data :amount)))
                                              result))
                                      tasks-aggregated)
                             (nreverse result))))
      ;; Sort
      (setq tasks-plain (sort tasks-plain (lambda (a b) (string< (plist-get a :date) (plist-get b :date)))))
      (setq aggregated-list (sort aggregated-list (lambda (a b) (string< (plist-get a :description) (plist-get b :description)))))
      ;; Final calculation
      (let* ((tax-amount (* subtotal (/ tax-rate 100.0)))
             (total-amount-eur (+ subtotal tax-amount))
             (total-amount-rsd (* total-amount-eur exchange-rate))
             (period (format-time-string "%Y-%m" (date-to-time start))))
        (list :client (plist-put client :NAME client-name)
              :start start
              :end end
              :state (or state 'draft)
              :invoice-id invoice-id
              :generated generated
              :due_date due-date
              :currency currency
              :tasks-plain tasks-plain
              :tasks-aggregated aggregated-list
              :subtotal (format "%.2f" subtotal)
              :tax-rate tax-rate
              :tax-amount (format "%.2f" tax-amount)
              :total (format "%.2f" total-amount-eur)
              :total_rsd (format "%.2f" total-amount-rsd)
              :exchange_rate (format "%.2f" exchange-rate)
              :payment_slip ip-invoice-include-payment-slip
              :period period
              :company company)))))

(defun ip-invoice--generate-html (invoice output-file)
  "Generate HTML invoice from INVOICE data to OUTPUT-FILE."
  (ip-debug-log 'info 'invoice "Generating HTML invoice: %s" output-file)
  (let* ((template-file (expand-file-name ip-invoice-template-file))
         (template (if (and (file-exists-p template-file)
                            (file-readable-p template-file))
                       (with-temp-buffer
                         (set-buffer-file-coding-system 'utf-8)
                         (insert-file-contents template-file)
                         (buffer-string))
                     (progn
                       (ip-debug-log 'error 'invoice "Template not found or unreadable: %s" template-file)
                       "<h1>Invoice Template Not Found</h1>")))
         (data (ip-invoice--convert-plist-to-mustache-data invoice)))
    (condition-case err
        (with-temp-file output-file
          (set-buffer-file-coding-system 'utf-8)
          (insert (mustache-render template data))
          (write-region (point-min) (point-max) output-file nil 'silent)
          (ip-debug-log 'success 'invoice "HTML invoice generated: %s" output-file))
      (error
       (ip-debug-log 'error 'invoice "Failed to generate HTML: %s" (error-message-string err))
       (error "Failed to generate HTML: %s" (error-message-string err))))))

(defun ip-invoice--convert-plist-to-mustache-data (plist)
  "Convert a PLIST with keyword keys to a Mustache-compatible alist."
  (let (result)
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (push (cons (substring (symbol-name key) 1)
                    (cond
                     ((and (listp value) (not (null value)) (keywordp (car value)))
                      (ip-invoice--convert-plist-to-mustache-data value))
                     ((and (listp value) (listp (car value)))
                      (mapcar #'ip-invoice--convert-plist-to-mustache-data value))
                     (t value)))
              result)
        (setq plist (cddr plist))))
    (nreverse result)))

(defun ip-invoice--last-day-of-month (year month)
  "Return last day of MONTH in YEAR as string YYYY-MM-DD."
  (let* ((time (encode-time 0 0 0 1 month year))
         (next-month (time-add time (days-to-time 32)))
         (next-month-num (string-to-number (format-time-string "%m" next-month))))
    (if (and (= year (string-to-number (format-time-string "%Y" next-month)))
             (= (mod (1- (+ month 1)) 12) (1- next-month-num)))
        (format-time-string "%Y-%m-%d" (time-subtract next-month (seconds-to-time 86400)))
      (format "%04d-%02d-%02d" year month (cadr (calendar-last-day-of-month month year))))))

(defun ip-invoice-generate (client-id start end &optional state)
  "Generate an invoice and save to file."
  (let* ((invoice (ip-invoice-generate-data client-id start end state))
         (output-dir (if (eq state 'final) ip-invoice-final-dir ip-invoice-draft-dir))
         (output-file (expand-file-name (format "%s-%s.html" client-id (plist-get invoice :invoice-id)) output-dir)))
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    (ip-debug-log 'debug 'invoice "tasks-plain type: %S, value: %S"
              (type-of (plist-get invoice :tasks-plain))
              (plist-get invoice :tasks-plain))
    (ip-debug-log 'debug 'invoice "tasks-aggregated type: %S, value: %S"
              (type-of (plist-get invoice :tasks-aggregated))
              (plist-get invoice :tasks-aggregated))
    (ip-debug-log 'debug 'invoice "invoice: %S" invoice)
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
    (ip-debug-log 'info 'invoice "Generating current month invoice for %s (%s to %s)" client-id start end)
    (let ((output-file (ip-invoice-generate client-id start end 'draft)))
      (message "Draft invoice generated: %s" output-file)
      (browse-url (concat "file://" output-file)))))

;;;###autoload
(defun ip-invoice-generate-interactive ()
  "Interactively generate an invoice."
  (interactive)
  (let* ((client-id (completing-read "Client ID: " (ip-list-client-ids)))
         (start (read-string "Start date (YYYY-MM-DD): "))
         (end (read-string "End date (YYYY-MM-DD): "))
;;         (type-str (completing-read "Invoice type: " '("task" "service") nil t))
;;         (invoice-type (intern type-str))
         (final-p (y-or-n-p "Generate final invoice? "))
         (state (if final-p 'final 'draft)))
    (unless (and (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" start)
                  (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" end))
      (user-error "Invalid date format. Use YYYY-MM-DD"))
    (let ((output-file (ip-invoice-generate client-id start end state)))
      (message "Invoice generated: %s" output-file)
      (browse-url (concat "file://" output-file)))))

(provide 'ip-invoice)
;;; ip-invoice.el ends here