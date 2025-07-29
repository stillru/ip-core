;;; ip-invoice.el --- Generate invoice data from org-mode tasks -*- lexical-binding: t; -*-

;;; Commentary:
;; This package generates invoices from Org-mode tasks with clock entries.
;; It uses tags to identify clients and services, matching headlines in `clients.org`.
;; Supports custom HTML templates via `mustache.el`.

;;; Code:

(require 'org-element)
(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'ip-core)
(require 'mustache)
(require 'subr-x)

;; Try to load ip-debug, provide fallbacks if not available
(condition-case nil
    (require 'ip-debug)
  (error
   ;; Define fallback functions if ip-debug is not available
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
     "Fallback debug macro that uses `message'."
     `(ip-debug-log 'info ,module ,message ,@args))))

;; Customization variables
(defgroup ip-invoice nil
  "Invoice generation for IP management system."
  :group 'ip-core)

(defcustom ip-invoice-final-dir (expand-file-name "invoices/2025/" ip-org-directory)
  "Directory to store finalized invoices."
  :type 'directory
  :group 'ip-invoice)

(defcustom ip-invoice-draft-dir (expand-file-name "invoices/drafts/" ip-org-directory)
  "Directory to store draft invoices."
  :type 'directory
  :group 'ip-invoice)

(defcustom ip-invoice-template-file nil
  "Path to custom invoice HTML template file. If nil, use default template."
  :type '(choice (file :tag "Template file")
                 (const :tag "Default template" nil))
  :group 'ip-invoice)

(defcustom ip-invoice-default-template
  "<html>
<head><title>Invoice</title>
<style>table { border-collapse: collapse; } td, th { border: 1px solid black; padding: 8px; } body { font-family: Arial; }</style>
</head>
<body>
<h1>Invoice for {{client.name}}</h1>
<p>Period: {{start}} to {{end}}</p>
<p>State: {{state}}</p>
<p>Client Address: {{client.address}}</p>
<p>Client Email: {{client.email}}</p>
<p>Payment Details: {{client.payment_details}}</p>
<table>
  <tr><th>Service</th><th>Hours</th><th>Rate</th><th>Amount</th></tr>
  {{#services}}
  <tr><td>{{description}}</td><td>{{hours}}</td><td>{{rate}}</td><td>{{amount}}</td></tr>
  {{/services}}
  {{#tax-rate}}
  <tr><td colspan=\"3\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
  <tr><td colspan=\"3\">Tax ({{tax-rate}}%)</td><td>{{tax-amount}} {{client.currency}}</td></tr>
  {{/tax-rate}}
  <tr><td colspan=\"3\">Total</td><td>{{total}} {{client.currency}}</td></tr>
</table>
</body>
</html>"
  "Default HTML template for invoices."
  :type 'string
  :group 'ip-invoice)

;; Invoice type customization
(defcustom ip-invoice-type 'service
  "Default invoice type. Can be \\='service or \\='task."
  :type '(choice (const :tag "Group by services" service)
                 (const :tag "Group by tasks" task))
  :group 'ip-invoice)

(defcustom ip-invoice-task-template
  "<html>
<head><title>Invoice {{invoice-id}}</title>
<style>table { border-collapse: collapse; } td, th { border: 1px solid black; padding: 8px; } body { font-family: Arial; } .task-date { background-color: #f0f0f0; font-weight: bold; }</style>
</head>
<body>
<h1>Invoice for {{client.name}}</h1>
<p>Period: {{start}} to {{end}}</p>
<p>State: {{state}}</p>
{{#if invoice-id}}
<p>Invoice ID: {{invoice-id}}</p>
{{/if}}
<p>Client Address: {{client.address}}</p>
<p>Client Email: {{client.email}}</p>
<p>Payment Details: {{client.payment_details}}</p>
<table>
  <tr><th>Date</th><th>Task</th><th>Hours</th><th>Rate</th><th>Amount</th></tr>
  {{#each tasks}}
  <tr><td>{{date}}</td><td>{{description}}</td><td>{{hours}}</td><td>{{rate}}</td><td>{{amount}}</td></tr>
  {{/each}}
  {{#if tax-rate}}
  <tr><td colspan=\"4\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
  <tr><td colspan=\"4\">Tax ({{tax-rate}}%)</td><td>{{tax-amount}} {{client.currency}}</td></tr>
  {{/if}}
  <tr><td colspan=\"4\">Total</td><td>{{total}} {{client.currency}}</td></tr>
</table>
</body>
</html>"
  "HTML template for task-based invoices."
  :type 'string
  :group 'ip-invoice)

;; Core functions
(defun ip--parse-timestamp (ts)
  "Convert org-element timestamp TS to YYYY-MM-DD string."
  (when (and ts (eq (org-element-type ts) 'timestamp))
    (condition-case nil
        (let ((raw-value (org-element-property :raw-value ts)))
          (if raw-value
              (format-time-string "%Y-%m-%d"
                                  (org-time-string-to-time raw-value))
            (progn
              (ip-debug-log 'warning 'invoice "Invalid timestamp: %S" ts)
              nil)))
      (error 
       (ip-debug-log 'error 'invoice "Invalid timestamp: %S" ts)
       nil))))

(defun ip--task-total-hours (task)
  "Calculate total clocked hours in TASK."
  (let ((durations (org-element-map task 'clock
                     (lambda (cl)
                       (when-let ((start-ts (org-element-property :value cl))
                                  (end-ts (org-element-property :end-time cl)))
                         (let ((start-str (org-element-property :raw-value start-ts))
                               (end-str (org-element-property :raw-value end-ts)))
                           (when (and start-str end-str)
                             (float-time
                              (time-subtract
                               (org-time-string-to-time end-str)
                               (org-time-string-to-time start-str))))))))))
    (if durations
        (/ (apply #'+ (cl-remove-if-not #'identity durations)) 3600.0)
      0.0)))

(defun ip--task-in-range-p (task start end)
  "Return t if TASK has any clock entries between START and END."
  (let* ((start-ts (date-to-time start))
         (end-ts (date-to-time end)))
    (cl-some
     (lambda (cl)
       (when-let* ((start-ts-obj (org-element-property :value cl))
                   (end-ts-obj (org-element-property :end-time cl))
                   (start-str (org-element-property :raw-value start-ts-obj))
                   (end-str (org-element-property :raw-value end-ts-obj))
                   (clock-start (org-time-string-to-time start-str))
                   (clock-end (org-time-string-to-time end-str)))
         (and (time-less-p clock-start end-ts)
              (time-less-p start-ts clock-end))))
     (org-element-map task 'clock #'identity))))

(defun ip--parse-task (task)
  "Extract relevant info from TASK headline."
  (condition-case err
      (let* ((props (org-element-property :properties task))
             (client (or (cdr (assoc-string "CLIENT" props t)) "unknown"))
             (service (or (cdr (assoc-string "REPO" props t)) "general"))
             (title (org-element-property :raw-value task))
             (hours (ip--task-total-hours task)))
        (list :title title :client client :service service :hours hours))
    (error 
     (ip-debug-log 'error 'invoice "Error parsing task: %s" (error-message-string err))
     nil)))

(defun ip--load-tasks-in-range (start end)
  "Return list of tasks as plists, filtered by clock range."
  (ip-debug-log 'info 'invoice "Loading tasks in range %s to %s" start end)
  (let ((ast (ip--load-org-file ip-tasks-file))
        (count 0))
    (prog1
        (cl-loop for hl in (org-element-map ast 'headline #'identity)
                 when (ip--task-in-range-p hl start end)
                 do (cl-incf count)
                 collect (ip--parse-task hl))
      (ip-debug-log 'success 'invoice "Loaded %d tasks in date range" count))))

(defun ip--generate-final-invoice-id ()
  "Generate next available final invoice ID based on existing files."
  (unless (file-directory-p ip-invoice-final-dir)
    (make-directory ip-invoice-final-dir t)
    (ip-debug-log 'info 'invoice "Created final invoice directory: %s" ip-invoice-final-dir))
  (let* ((files (directory-files ip-invoice-final-dir nil "^invoice-\\([0-9]+\\)\\.org$"))
         (nums (mapcar (lambda (f)
                         (string-to-number (cadr (split-string f "[-.]"))))
                       files))
         (next (1+ (apply #'max 0 nums)))
         (invoice-id (format "INV-2025-%04d" next)))
    (ip-debug-log 'info 'invoice "Generated invoice ID: %s" invoice-id)
    invoice-id))

(defun ip--get-task-clock-entries (task start end)
  "Get clock entries for TASK within START and END date range."
  (let* ((start-ts (date-to-time start))
         (end-ts (date-to-time end)))
    (org-element-map task 'clock
      (lambda (cl)
        (when-let* ((range-str (org-element-property :value cl))
                    (parts (split-string range-str "--"))
                    (start-str (string-trim (car parts)))
                    (end-str (string-trim (cadr parts)))
                    (clock-start (org-time-string-to-time start-str))
                    (clock-end (org-time-string-to-time end-str)))
          (when (and (time-less-p clock-start end-ts)
                     (time-less-p start-ts clock-end))
            (list :start clock-start
                  :end clock-end 
                  :date (format-time-string "%Y-%m-%d" clock-start)
                  :hours (/ (float-time (time-subtract clock-end clock-start)) 3600.0))))))))

(defun ip--parse-task-detailed (task)
  "Extract detailed info from TASK headline including all clock entries."
  (condition-case err
      (let* ((props (org-element-property :properties task))
             (client (or (cdr (assoc-string "CLIENT" props t)) "unknown"))
             (service (or (cdr (assoc-string "REPO" props t)) "general"))
             (title (org-element-property :raw-value task)))
        (list :title title :client client :service service :element task))
    (error 
     (ip-debug-log 'error 'invoice "Error parsing detailed task: %s" (error-message-string err))
     nil)))

(defun ip--load-tasks-detailed (start end)
  "Return list of tasks with detailed clock info, filtered by clock range."
  (ip-debug-log 'info 'invoice "Loading detailed tasks in range %s to %s" start end)
  (let ((ast (ip--load-org-file ip-tasks-file))
        (count 0))
    (prog1
        (cl-loop for hl in (org-element-map ast 'headline #'identity)
                 when (ip--task-in-range-p hl start end)
                 do (cl-incf count)
                 collect (ip--parse-task-detailed hl))
      (ip-debug-log 'success 'invoice "Loaded %d detailed tasks in date range" count))))

(defun ip-generate-invoice-data (client-id start end &optional state invoice-type)
  "Generate invoice data for CLIENT-ID between START and END.
INVOICE-TYPE can be \\='service (default) or \\='task."
  (ip-debug-log 'info 'invoice "Generating invoice for client %s (%s to %s, type: %s)" 
                client-id start end (or invoice-type 'service))
  (unless (ip-get-client-by-id client-id)
    (ip-debug-log 'error 'invoice "Unknown client ID: %s" client-id)
    (error "Unknown client ID: %s" client-id))
  (let* ((invoice-type (or invoice-type ip-invoice-type))
         (client (ip-get-client-by-id client-id))
         (currency (or (plist-get client :CURRENCY) "EUR"))
         (tax-rate (or (plist-get client :TAX_RATE) nil))
         (invoice-id (when (eq state 'final)
                       (ip--generate-final-invoice-id))))

    (cond
     ;; Service-based invoice (original logic)
     ((eq invoice-type 'service)
      (ip-debug-log 'info 'invoice "Processing service-based invoice")
      (let* ((tasks (ip--load-tasks-in-range start end))
             (filtered (cl-remove-if-not (lambda (task)
                                           (and task (string= (plist-get task :client) client-id)))
                                         tasks))
             (grouped (make-hash-table :test 'equal))
             (services ())
             (subtotal 0.0))
        (ip-debug-log 'info 'invoice "Found %d tasks for client %s" (length filtered) client-id)
        ;; Group tasks by service
        (dolist (task filtered)
          (let ((service (or (plist-get task :service) "general")))
            (puthash service
                     (+ (gethash service grouped 0.0)
                        (plist-get task :hours))
                     grouped)))
        ;; Calculate service line items
        (maphash (lambda (svc hours)
                   (let* ((svc-data (ip-get-client-service client-id svc))
                          (rate (string-to-number (or (plist-get svc-data :RATE) "0")))
                          (amount (* rate hours))
                          (taxable (plist-get svc-data :TAXABLE)))
                     (setq subtotal (+ subtotal amount))
                     (push (list :description (or (plist-get svc-data :DESCRIPTION) svc)
                                 :hours (format "%.2f" hours)
                                 :rate (format "%.2f" rate)
                                 :amount (format "%.2f" amount)
                                 :taxable taxable)
                           services)
                     (ip-debug-log 'info 'invoice "Service %s: %.2f hours @ %.2f = %.2f" 
                                   svc hours rate amount)))
                 grouped)
        ;; Calculate taxes and total
        (let* ((tax-amount (if tax-rate (* subtotal (/ tax-rate 100.0)) 0.0))
               (total (+ subtotal tax-amount)))
          (ip-debug-log 'success 'invoice "Service invoice generated: subtotal %.2f, tax %.2f, total %.2f" 
                        subtotal tax-amount total)
          (list :client client
                :start start
                :end end
                :currency currency
                :state (or state 'draft)
                :invoice-id invoice-id
                :type 'service
                :services (nreverse services)
                :subtotal (format "%.2f" subtotal)
                :tax-rate tax-rate
                :tax-amount (format "%.2f" tax-amount)
                :total (format "%.2f" total)))))

     ;; Task-based invoice (new logic)
     ((eq invoice-type 'task)
      (ip-debug-log 'info 'invoice "Processing task-based invoice")
      (let* ((tasks (ip--load-tasks-detailed start end))
             (filtered (cl-remove-if-not (lambda (task)
                                           (and task (string= (plist-get task :client) client-id)))
                                         tasks))
             (task-entries ())
             (subtotal 0.0))
        (ip-debug-log 'info 'invoice "Found %d detailed tasks for client %s" (length filtered) client-id)
        ;; Process each task's clock entries
        (dolist (task filtered)
          (let* ((task-element (plist-get task :element))
                 (task-title (plist-get task :title))
                 (service (plist-get task :service))
                 (svc-data (ip-get-client-service client-id service))
                 (rate (string-to-number (or (plist-get svc-data :RATE) "0")))
                 (clock-entries (ip--get-task-clock-entries task-element start end)))
            (dolist (entry clock-entries)
              (let* ((hours (plist-get entry :hours))
                     (amount (* rate hours)))
                (setq subtotal (+ subtotal amount))
                (push (list :date (plist-get entry :date)
                            :description task-title
                            :hours (format "%.2f" hours)
                            :rate (format "%.2f" rate)
                            :amount (format "%.2f" amount))
                      task-entries)
                (ip-debug-log 'info 'invoice "Task entry %s: %s - %.2f hours @ %.2f = %.2f" 
                              (plist-get entry :date) task-title hours rate amount)))))
        ;; Sort by date
        (setq task-entries (sort task-entries
                                 (lambda (a b) (string< (plist-get a :date) (plist-get b :date)))))
        ;; Calculate taxes and total
        (let* ((tax-amount (if tax-rate (* subtotal (/ tax-rate 100.0)) 0.0))
               (total (+ subtotal tax-amount)))
          (ip-debug-log 'success 'invoice "Task invoice generated: %d entries, subtotal %.2f, tax %.2f, total %.2f" 
                        (length task-entries) subtotal tax-amount total)
          (list :client client
                :start start
                :end end
                :currency currency
                :state (or state 'draft)
                :invoice-id invoice-id
                :type 'task
                :tasks task-entries
                :subtotal (format "%.2f" subtotal)
                :tax-rate tax-rate
                :tax-amount (format "%.2f" tax-amount)
                :total (format "%.2f" total)))))

     (t 
      (ip-debug-log 'error 'invoice "Unknown invoice type: %s" invoice-type)
      (error "Unknown invoice type: %s" invoice-type)))))

;;;###autoload
(defun ip-preview-invoice-text (client-id start end &optional state invoice-type)
  "Show textual invoice preview for CLIENT-ID between START and END."
  (interactive
   (list
    (completing-read "Client ID: " (ip-list-client-ids))
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")
    nil
    (intern (completing-read "Invoice type: " '("service" "task") nil t))))
  (ip-debug-log 'info 'invoice "Generating text preview for client %s" client-id)
  (let* ((invoice (ip-generate-invoice-data client-id start end state invoice-type))
         (buf (get-buffer-create "*Invoice Preview*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Invoice for %s\n"
                      (or (plist-get (plist-get invoice :client) :NAME)
                          (plist-get invoice :client))))
      (insert (format "Period: %s to %s\n" start end))
      (insert (format "State: %s\n" (plist-get invoice :state)))
      (insert (format "Type: %s\n" (plist-get invoice :type)))
      (when-let ((id (plist-get invoice :invoice-id)))
        (insert (format "Invoice ID: %s\n" id)))

      ;; Display items based on invoice type
      (cond
       ((eq (plist-get invoice :type) 'service)
        (insert "\nServices:\n")
        (dolist (svc (plist-get invoice :services))
          (insert (format "- %s: %s hours @ %s/%s = %s %s\n"
                          (plist-get svc :description)
                          (plist-get svc :hours)
                          (plist-get svc :rate)
                          (plist-get invoice :currency)
                          (plist-get svc :amount)
                          (plist-get invoice :currency)))))
       ((eq (plist-get invoice :type) 'task)
        (insert "\nTasks:\n")
        (dolist (task (plist-get invoice :tasks))
          (insert (format "- %s: %s (%s hours @ %s/%s = %s %s)\n"
                          (plist-get task :date)
                          (plist-get task :description)
                          (plist-get task :hours)
                          (plist-get task :rate)
                          (plist-get invoice :currency)
                          (plist-get task :amount)
                          (plist-get invoice :currency))))))

      (when (plist-get invoice :tax-rate)
        (insert (format "\nSubtotal: %s %s\n"
                        (plist-get invoice :subtotal)
                        (plist-get invoice :currency)))
        (insert (format "Tax (%s%%): %s %s\n"
                        (plist-get invoice :tax-rate)
                        (plist-get invoice :tax-amount)
                        (plist-get invoice :currency))))
      (insert (format "Total: %s %s\n"
                      (plist-get invoice :total)
                      (plist-get invoice :currency)))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf)
    (ip-debug-log 'success 'invoice "Text preview generated successfully")))

(defun ip-generate-invoice-html (invoice-data output-file)
  "Generate HTML invoice from INVOICE-DATA to OUTPUT-FILE."
  (ip-debug-log 'info 'invoice "Generating HTML invoice: %s" output-file)
  (let* ((invoice-type (plist-get invoice-data :type))
         (template (cond
                    ((eq invoice-type 'task)
                     (if (and ip-invoice-template-file
                              (file-exists-p ip-invoice-template-file))
                         (with-temp-buffer
                           (insert-file-contents ip-invoice-template-file)
                           (buffer-string))
                       ip-invoice-task-template))
                    (t
                     (if (and ip-invoice-template-file
                              (file-exists-p ip-invoice-template-file))
                         (with-temp-buffer
                           (insert-file-contents ip-invoice-template-file)
                           (buffer-string))
                       ip-invoice-default-template))))
         (data (list :invoice-id (or (plist-get invoice-data :invoice-id) "")
                     :client (list :name (or (plist-get (plist-get invoice-data :client) :NAME)
                                             (plist-get invoice-data :client))
                                   :address (or (plist-get (plist-get invoice-data :client) :ADDRESS) "")
                                   :email (or (plist-get (plist-get invoice-data :client) :EMAIL) "")
                                   :payment_details (or (plist-get (plist-get invoice-data :client) :PAYMENT_DETAILS) "")
                                   :currency (plist-get invoice-data :currency))
                     :start (plist-get invoice-data :start)
                     :end (plist-get invoice-data :end)
                     :state (symbol-name (plist-get invoice-data :state))
                     :services (plist-get invoice-data :services)
                     :tasks (plist-get invoice-data :tasks)
                     :subtotal (plist-get invoice-data :subtotal)
                     :tax-rate (plist-get invoice-data :tax-rate)
                     :tax-amount (plist-get invoice-data :tax-amount)
                     :total (plist-get invoice-data :total))))
    (condition-case err
        (progn
          (with-temp-file output-file
            (insert (mustache-render template data)))
          (ip-debug-log 'success 'invoice "HTML invoice generated: %s" output-file))
      (error
       (ip-debug-log 'error 'invoice "Failed to generate HTML invoice: %s" (error-message-string err))
       (error "Failed to generate HTML invoice: %s" (error-message-string err))))))

;;;###autoload
(defun ip-create-invoice (client-id start end &optional final invoice-type)
  "Create invoice for CLIENT-ID between START and END.
If FINAL is non-nil, create final invoice with unique ID.
INVOICE-TYPE can be \\='service (default) or \\='task."
  (interactive
   (list
    (completing-read "Client ID: " (ip-list-client-ids))
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")
    (y-or-n-p "Final invoice? ")
    (intern (completing-read "Invoice type: " '("service" "task") nil t))))
  (ip-debug-log 'info 'invoice "Creating %s invoice for %s (%s to %s)" 
                (if final "final" "draft") client-id start end)
  (let* ((state (if final 'final 'draft))
         (invoice-type (or invoice-type 'service))
         (invoice (ip-generate-invoice-data client-id start end state invoice-type))
         (output-dir (if final ip-invoice-final-dir ip-invoice-draft-dir))
         (type-suffix (if (eq invoice-type 'task) "-tasks" ""))
         (filename (format "%sinvoice-%s%s.html" output-dir
                           (or (plist-get invoice :invoice-id)
                               (format "%s-%s" client-id (format-time-string "%Y%m%d")))
                           type-suffix)))
    (unless (file-directory-p output-dir)
      (make-directory output-dir t)
      (ip-debug-log 'info 'invoice "Created output directory: %s" output-dir))
    (ip-generate-invoice-html invoice filename)
    (ip-debug-log 'success 'invoice "Invoice created successfully: %s" filename)
    (message "Invoice created: %s" filename)))

;;;###autoload
(defun ip-create-both-invoices (client-id start end &optional final)
  "Create both service and task invoices for CLIENT-ID between START and END."
  (interactive
   (list
    (completing-read "Client ID: " (ip-list-client-ids))
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")
    (y-or-n-p "Final invoices? ")))
  (ip-debug-log 'info 'invoice "Creating both invoice types for %s (%s)" 
                client-id (if final "final" "draft"))
  (ip-create-invoice client-id start end final 'service)
  (ip-create-invoice client-id start end final 'task)
  (ip-debug-log 'success 'invoice "Both invoices created for %s" client-id)
  (message "Both service and task invoices created for %s" client-id))

(provide 'ip-invoice)
;;; ip-invoice.el ends here