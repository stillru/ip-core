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
<head><title>Invoice {{invoice-id}}</title>
<style>table { border-collapse: collapse; } td, th { border: 1px solid black; padding: 8px; } body { font-family: Arial; }</style>
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
  <tr><th>Service</th><th>Hours</th><th>Rate</th><th>Amount</th></tr>
  {{#each services}}
  <tr><td>{{description}}</td><td>{{hours}}</td><td>{{rate}}</td><td>{{amount}}</td></tr>
  {{/each}}
  {{#if tax-rate}}
  <tr><td colspan=\"3\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
  <tr><td colspan=\"3\">Tax ({{tax-rate}}%)</td><td>{{tax-amount}} {{client.currency}}</td></tr>
  {{/if}}
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
              (message "Invalid timestamp: %S" ts)
              nil)))
      (error (message "Invalid timestamp: %S" ts)
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
         (end-ts (date-to-time end))
         (clocks (org-element-map task 'clock
                   (lambda (cl)
                     (when-let ((start-time (org-element-property :value cl))
                                (end-time (org-element-property :end-time cl)))
                       (let ((start-str (org-element-property :raw-value start-time))
                             (end-str (org-element-property :raw-value end-time)))
                         (when (and start-str end-str)
                           (cons (org-time-string-to-time start-str)
                                 (org-time-string-to-time end-str)))))))))
    (cl-some (lambda (pair)
               (and pair
                    (time-less-p (car pair) end-ts)
                    (time-less-p start-ts (cdr pair))))
             clocks)))

(defun ip--parse-task (task)
  "Extract relevant info from TASK headline."
  (condition-case err
      (let* ((tags (mapcar #'downcase (org-element-property :tags task)))
             (client (or (cl-find-if (lambda (tag) (member tag (ip-list-client-ids))) tags) "unknown"))
             (service (or (cl-find-if (lambda (tag) (member tag (ip-list-service-tags))) tags) "general"))
             (title (org-element-property :raw-value task))
             (hours (ip--task-total-hours task)))
        (list :title title :client client :service service :hours hours))
    (error (message "Error parsing task: %s" (error-message-string err))
           nil)))

(defun ip--load-tasks-in-range (start end)
  "Return list of tasks as plists, filtered by clock range."
  (let ((ast (ip--load-org-file ip-tasks-file)))
    (cl-loop for hl in (org-element-map ast 'headline #'identity)
             when (ip--task-in-range-p hl start end)
             collect (ip--parse-task hl))))

(defun ip--generate-final-invoice-id ()
  "Generate next available final invoice ID based on existing files."
  (unless (file-directory-p ip-invoice-final-dir)
    (make-directory ip-invoice-final-dir t))
  (let* ((files (directory-files ip-invoice-final-dir nil "^invoice-\\([0-9]+\\)\\.org$"))
         (nums (mapcar (lambda (f)
                         (string-to-number (cadr (split-string f "[-.]"))))
                       files))
         (next (1+ (apply #'max 0 nums))))
    (format "INV-2025-%04d" next)))

(defun ip--get-task-clock-entries (task start end)
  "Get clock entries for TASK within START and END date range."
  (let* ((start-ts (date-to-time start))
         (end-ts (date-to-time end)))
    (org-element-map task 'clock
      (lambda (cl)
        (when-let ((start-time (org-element-property :value cl))
                   (end-time (org-element-property :end-time cl)))
          (let ((start-str (org-element-property :raw-value start-time))
                (end-str (org-element-property :raw-value end-time)))
            (when (and start-str end-str)
              (let ((clock-start (org-time-string-to-time start-str))
                    (clock-end (org-time-string-to-time end-str)))
                (when (and (time-less-p clock-start end-ts)
                           (time-less-p start-ts clock-end))
                  (list :start clock-start
                        :end clock-end 
                        :date (format-time-string "%Y-%m-%d" clock-start)
                        :hours (/ (float-time (time-subtract clock-end clock-start)) 3600.0)))))))))))

(defun ip--parse-task-detailed (task)
  "Extract detailed info from TASK headline including all clock entries."
  (condition-case err
      (let* ((tags (mapcar #'downcase (org-element-property :tags task)))
             (client (or (cl-find-if (lambda (tag) (member tag (ip-list-client-ids))) tags) "unknown"))
             (service (or (cl-find-if (lambda (tag) (member tag (ip-list-service-tags))) tags) "general"))
             (title (org-element-property :raw-value task)))
        (list :title title :client client :service service :element task))
    (error (message "Error parsing task: %s" (error-message-string err))
           nil)))

(defun ip--load-tasks-detailed (start end)
  "Return list of tasks with detailed clock info, filtered by clock range."
  (let ((ast (ip--load-org-file ip-tasks-file)))
    (cl-loop for hl in (org-element-map ast 'headline #'identity)
             when (ip--task-in-range-p hl start end)
             collect (ip--parse-task-detailed hl))))

(defun ip-generate-invoice-data (client-id start end &optional state invoice-type)
  "Generate invoice data for CLIENT-ID between START and END.
INVOICE-TYPE can be \\='service (default) or \\='task."
  (unless (ip-get-client-by-id client-id)
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
      (let* ((tasks (ip--load-tasks-in-range start end))
             (filtered (cl-remove-if-not (lambda (task)
                                           (and task (string= (plist-get task :client) client-id)))
                                         tasks))
             (grouped (make-hash-table :test 'equal))
             (services ())
             (subtotal 0.0))
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
                           services)))
                 grouped)
        ;; Calculate taxes and total
        (let* ((tax-amount (if tax-rate (* subtotal (/ tax-rate 100.0)) 0.0))
               (total (+ subtotal tax-amount)))
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
      (let* ((tasks (ip--load-tasks-detailed start end))
             (filtered (cl-remove-if-not (lambda (task)
                                           (and task (string= (plist-get task :client) client-id)))
                                         tasks))
             (task-entries ())
             (subtotal 0.0))
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
                      task-entries)))))
        ;; Sort by date
        (setq task-entries (sort task-entries
                                 (lambda (a b) (string< (plist-get a :date) (plist-get b :date)))))
        ;; Calculate taxes and total
        (let* ((tax-amount (if tax-rate (* subtotal (/ tax-rate 100.0)) 0.0))
               (total (+ subtotal tax-amount)))
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
     
     (t (error "Unknown invoice type: %s" invoice-type)))))

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
    (display-buffer buf)))

(defun ip-generate-invoice-html (invoice-data output-file)
  "Generate HTML invoice from INVOICE-DATA to OUTPUT-FILE."
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
    (with-temp-file output-file
      (insert (mustache-render template data)))))

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
      (make-directory output-dir t))
    (ip-generate-invoice-html invoice filename)
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
  (ip-create-invoice client-id start end final 'service)
  (ip-create-invoice client-id start end final 'task)
  (message "Both service and task invoices created for %s" client-id))

(provide 'ip-invoice)
;;; ip-invoice.el ends here