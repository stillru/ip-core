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

(defvar ip-invoice-final-dir (expand-file-name "invoices/2025/" ip-org-directory)
  "Directory to store finalized invoices.")

(defvar ip-invoice-draft-dir (expand-file-name "invoices/drafts/" ip-org-directory)
  "Directory to store draft invoices.")

(defvar ip-invoice-template-file nil
  "Path to custom invoice HTML template file. If nil, use default template.")

(defvar ip-invoice-default-template
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
  <tr><th>Service</th><th>Count</th><th>Price</th><th>Amount</th></tr>
  {{#each services}}
  <tr><td>{{description}}</td><td>{{count}}</td><td>{{price}}</td><td>{{amount}}</td></tr>
  {{/each}}
  <tr><td colspan=\"3\">Total</td><td>{{total}} {{client.currency}}</td></tr>
</table>
</body>
</html>"
  "Default HTML template for invoices.")

(defun ip--parse-timestamp (ts)
  "Convert org-element timestamp TS to YYYY-MM-DD string."
  (when ts
    (format-time-string "%Y-%m-%d" (org-time-string-to-time (org-element-property :raw-value ts)))))

(defun ip--task-in-range-p (task start end)
  "Return t if TASK has any clock entries between START and END (YYYY-MM-DD)."
  (let ((start-ts (date-to-time start))
        (end-ts (date-to-time end))
        (clocks (org-element-map task 'clock
                  (lambda (cl)
                    (cons (org-time-string-to-time (org-element-property :value cl))
                          (org-time-string-to-time (org-element-property :end-time cl)))))))
    (cl-some (lambda (pair)
               (and (time-less-p (car pair) end-ts)
                    (time-less-p start-ts (cdr pair))))
             clocks)))

(defun ip--task-total-hours (task)
  "Calculate total clocked hours in TASK."
  (let ((duration (org-element-map task 'clock
                    (lambda (cl)
                      (float-time
                       (time-subtract
                        (org-time-string-to-time (org-element-property :end-time cl))
                        (org-time-string-to-time (org-element-property :value cl))))))))
    (/ (apply #'+ (or duration '(0))) 3600.0)))

(defun ip--parse-task (task)
  "Extract relevant info from TASK headline using tags for client and service."
  (let* ((clients (ip-get-clients))
         (client-ids (mapcar (lambda (c) (plist-get c :ID)) clients))
         (service-tags (cl-loop for c in clients
                                append (mapcar (lambda (s) (plist-get s :tag)) (plist-get c :services))))
         (tags (mapcar #'downcase (org-element-property :tags task)))
         (client (or (cl-find-if (lambda (tag) (member tag client-ids)) tags) "unknown"))
         (service (or (cl-find-if (lambda (tag) (member tag service-tags)) tags) "general"))
         (title (org-element-property :raw-value task))
         (hours (ip--task-total-hours task)))
    (list :title title :client client :service service :hours hours)))

(defun ip--find-client-service-data (client-id service)
  "Find service data (:description, :price, :count) for SERVICE under CLIENT-ID."
  (let* ((clients (ip-get-clients))
         (client (cl-find-if (lambda (c)
                               (string= (plist-get c :ID) client-id))
                             clients))
         (services (plist-get client :services))
         (service-data (cl-find-if (lambda (s)
                                     (string= (plist-get s :tag) service))
                                   services)))
    (or service-data
        (list :description service :price "0" :count "1"))))

(defun ip--load-tasks-in-range (start end)
  "Return list of tasks as plists, filtered by clock range."
  (let ((ast (ip--load-org-file ip-tasks-file)))
    (cl-loop for hl in (org-element-map ast 'headline #'identity)
             when (ip--task-in-range-p hl start end)
             collect (ip--parse-task hl))))

(defun ip-generate-invoice-data (client-id start end &optional state)
  "Generate invoice as plist for CLIENT-ID from tasks between START and END.
STATE can be \\='draft or \\='final. Dates are in YYYY-MM-DD format."
  (let* ((tasks (ip--load-tasks-in-range start end))
         (filtered (cl-remove-if-not (lambda (task)
                                       (string= (plist-get task :client) client-id))
                                     tasks))
         (grouped (make-hash-table :test 'equal)))
    (dolist (task filtered)
      (let ((service (or (plist-get task :service) "general")))
        (puthash service
                 (+ (gethash service grouped 0.0)
                    (plist-get task :hours))
                 grouped)))
    (let* ((client (cl-find-if (lambda (c)
                                 (string= (plist-get c :ID) client-id))
                               (ip-get-clients)))
           (currency (or (plist-get client :CURRENCY) "EUR"))
           (services ())
           (invoice-id (when (eq state 'final)
                         (ip--generate-final-invoice-id))))
      (maphash (lambda (svc hours)
                 (let* ((service-data (ip--find-client-service-data client-id svc))
                        (price (string-to-number (or (plist-get service-data :PRICE) "0")))
                        (count (string-to-number (or (plist-get service-data :COUNT) "1")))
                        (description (or (plist-get service-data :DESCRIPTION) svc))
                        (amount (* price count)))
                   (push (list :description description :count count :price price :amount amount :hours hours) services)))
               grouped)
      (list :client client
            :start start :end end
            :currency currency
            :state (or state 'draft)
            :invoice-id invoice-id
            :services (nreverse services)
            :total (apply #'+ (mapcar (lambda (s) (plist-get s :amount)) services))))))

(defun ip--generate-final-invoice-id ()
  "Generate next available final invoice ID based on existing files."
  (let* ((files (directory-files ip-invoice-final-dir nil "^invoice-\\([0-9]+\\)\\.org$"))
         (nums (mapcar (lambda (f)
                         (string-to-number (cadr (split-string f "[-.]"))))
                       files))
         (next (1+ (apply #'max 0 nums))))
    (format "INV-2025-%04d" next)))

(defun ip-preview-invoice-text (client-id start end &optional state)
  "Show a textual invoice buffer for CLIENT-ID from START to END.
STATE is \\='draft or \\='final."
  (interactive "sClient ID: \nsStart date (YYYY-MM-DD): \nsEnd date (YYYY-MM-DD): ")
  (let* ((invoice (ip-generate-invoice-data client-id start end state))
         (buf (get-buffer-create "*Invoice Preview*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Invoice for %s\n" (plist-get (plist-get invoice :client) :NAME)))
      (insert (format "Period: %s to %s\n" start end))
      (insert (format "State: %s\n" (plist-get invoice :state)))
      (when-let ((id (plist-get invoice :invoice-id)))
        (insert (format "Invoice ID: %s\n" id)))
      (insert (format "Client Address: %s\n" (plist-get (plist-get invoice :client) :ADDRESS)))
      (insert (format "Client Email: %s\n" (plist-get (plist-get invoice :client) :EMAIL)))
      (insert (format "Payment Details: %s\n" (plist-get (plist-get invoice :client) :PAYMENT_DETAILS)))
      (insert "\n")
      (insert (format "| %-20s | %-5s | %-7s | %-10s |\n" "Service" "Count" "Price" "Amount"))
      (insert (make-string 65 ?-) "\n")
      (dolist (svc (plist-get invoice :services))
        (insert (format "| %-20s | %5.2f | %7.2f | %10.2f |\n"
                        (plist-get svc :description)
                        (plist-get svc :count)
                        (plist-get svc :price)
                        (plist-get svc :amount))))
      (insert (make-string 65 ?-) "\n")
      (insert (format "| TOTAL%46s | %10.2f |\n" "" (plist-get invoice :total)))
      (goto-char (point-min))
      (read-only-mode t))
    (display-buffer buf)))

(defun ip-generate-invoice-html (invoice-data output-file)
  "Generate HTML invoice from INVOICE-DATA and save to OUTPUT-FILE."
  (let* ((template (if (and ip-invoice-template-file (file-exists-p ip-invoice-template-file))
                      (with-temp-buffer
                        (insert-file-contents ip-invoice-template-file)
                        (buffer-string))
                    ip-invoice-default-template))
         (data (list :invoice-id (or (plist-get invoice-data :invoice-id) "")
                     :client (list :name (or (plist-get (plist-get invoice-data :client) :NAME)
                                             (plist-get invoice-data :client))
                                   :address (or (plist-get (plist-get invoice-data :client) :ADDRESS) "")
                                   :email (or (plist-get (plist-get invoice-data :client) :EMAIL) "")
                                   :payment_details (or (plist-get (plist-get invoice-data :client) :PAYMENT_DETAILS) "")
                                   :currency (or (plist-get (plist-get invoice-data :client) :CURRENCY) "EUR"))
                     :start (plist-get invoice-data :start)
                     :end (plist-get invoice-data :end)
                     :state (symbol-name (plist-get invoice-data :state))
                     :services (mapcar (lambda (s)
                                         (list :description (plist-get s :description)
                                               :count (format "%.2f" (plist-get s :count))
                                               :price (format "%.2f" (plist-get s :price))
                                               :amount (format "%.2f" (plist-get s :amount))))
                                       (plist-get invoice-data :services))
                     :total (format "%.2f" (plist-get invoice-data :total)))))
    (with-temp-file output-file
      (insert (mustache-render template data)))))

(provide 'ip-invoice)
;;; ip-invoice.el ends here
