;;; ip-invoice.el --- Generate invoices from org-mode tasks -*- coding: utf-8 lexical-binding: t; -*-

;;; Commentary:
;; This module generates invoices from org-mode tasks for the IP management system.
;; Supports service-based and task-based invoices using org-mode clock entries.
;; Integrates with ip-core.el for client data and ip-debug.el for logging.
;; Outputs HTML invoices using mustache.el templates.
;; Enhanced with Serbian invoice format support.

;;; Code:

(eval-and-compile
  ;; Define fallback logging functions to satisfy compiler
  (unless (fboundp 'ip-debug-log)
    (defun ip-debug-log (level module message &rest args)
      "Fallback logging function that uses `message'.
LEVEL is the log level (\\='info, \\='success, \\='warning, \\='error).
MODULE is the module name (symbol).
MESSAGE is the format string, followed by ARGS."
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
      "Fallback debug macro that uses `ip-debug-log'.
MODULE is the module name (symbol).
MESSAGE is the format string, followed by ARGS."
      `(ip-debug-log 'info ,module ,message ,@args))))

(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'ip-core)
(require 'mustache)
(require 'subr-x)

;; Attempt to load ip-debug for full functionality
(condition-case nil
    (require 'ip-debug)
  (error
   (ip-debug-log 'warning 'invoice "Failed to load ip-debug.el, using fallback logging")))

;;; Customization
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
  "Path to custom HTML invoice template. If nil, uses default template."
  :type '(choice (file :tag "Template file")
                 (const :tag "Default template" nil))
  :group 'ip-invoice)

(defcustom ip-invoice-type 'service
  "Default invoice type: \\='service (group by services) or \\='task (group by tasks)."
  :type '(choice (const :tag "Group by services" service)
                 (const :tag "Group by tasks" task))
  :group 'ip-invoice)

;; Serbian invoice format customizations
(defcustom ip-invoice-company-info nil
  "Company information for invoices.
Should be a plist with keys like:
:name, :address, :email, :pib, :maticni_broj, :iban, :model, :poziv_base, :logo"
  :type 'plist
  :group 'ip-invoice)

(defcustom ip-invoice-template-choice 'enhanced
  "Choice of template to use for invoices.
Possible values: \='default, \='enhanced, or \='file."
  :type '(choice (const :tag "Default template" default)
                 (const :tag "Enhanced template" enhanced)
                 (const :tag "Custom template file" file))
  :group 'ip-invoice)

(defcustom ip-invoice-exchange-rate nil
  "EUR to RSD exchange rate. If nil, no conversion is shown."
  :type '(choice (number :tag "Exchange rate")
                 (const :tag "No conversion" nil))
  :group 'ip-invoice)

(defcustom ip-invoice-include-payment-slip nil
  "Whether to include Serbian payment slip in invoices."
  :type 'boolean
  :group 'ip-invoice)

(defcustom ip-invoice-enhanced-template
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Invoice {{invoice-id}}</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }
        .invoice-container {
            max-width: 800px;
            margin: 0 auto;
            background: white;
            padding: 40px;
            box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        .header {
            display: flex;
            justify-content: space-between;
            margin-bottom: 30px;
            align-items: flex-start;
        }
        .company-info h1 {
            margin: 0 0 10px 0;
            color: #2c3e50;
        }
        .company-info p {
            margin: 5px 0;
            color: #666;
        }
        .client-info {
            text-align: right;
        }
        .client-info p {
            margin: 5px 0;
            color: #666;
        }
        .details {
            display: flex;
            justify-content: space-between;
            margin-bottom: 30px;
            padding: 20px;
            background: #f8f9fa;
            border-radius: 5px;
        }
        .details p {
            margin: 5px 0;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin-bottom: 30px;
        }
        th, td {
            border: 1px solid #ddd;
            padding: 12px;
            text-align: left;
        }
        th {
            background-color: #34495e;
            color: white;
            font-weight: bold;
        }
        tr:nth-child(even) {
            background-color: #f2f2f2;
        }
        tr.total {
            background-color: #e8f4fd;
            font-weight: bold;
        }
        .total {
            text-align: right;
            margin-bottom: 30px;
        }
        .total p {
            margin: 5px 0;
            font-size: 16px;
        }
        .payment-slip {
            border: 2px solid #000;
            margin: 30px 0;
            font-size: 12px;
        }
        .payment-slip .row {
            display: flex;
            border-bottom: 1px solid #000;
        }
        .payment-slip .row:last-child {
            border-bottom: none;
        }
        .payment-slip .col {
            border-right: 1px solid #000;
            padding: 8px;
            flex: 1;
        }
        .payment-slip .col:last-child {
            border-right: none;
        }
        .payment-slip label {
            display: block;
            font-weight: bold;
            margin-bottom: 5px;
            font-size: 10px;
        }
        .payment-slip .uplatilac {
            flex: 2;
        }
        .payment-slip .svrha {
            flex: 2;
        }
        .payment-slip .primalac {
            flex: 2;
        }
        .payment-slip .potpis {
            flex: 2;
        }
        .payment-slip .qr {
            flex: 1;
            text-align: center;
        }
        .payment-slip .qr img {
            max-width: 80px;
            max-height: 80px;
        }
        .potpis-line {
            border-bottom: 1px solid #000;
            height: 20px;
            margin: 10px 0;
        }
        .payment-slip em {
            font-size: 10px;
            color: #666;
        }
        .footer {
            text-align: center;
            margin-top: 30px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            color: #666;
            font-size: 12px;
        }
        @media print {
            body { background: white; }
            .invoice-container { box-shadow: none; }
        }
    </style>
</head>
<body>
    <div class=\"invoice-container\">
        <div class=\"header\">
            <div>
                {{#company.logo}}
                <img src=\"{{company.logo}}\" alt=\"{{company.name}}\" style=\"max-width: 150px;\">
                {{/company.logo}}
                {{^company.logo}}
                <div style=\"width: 150px; height: 60px; border: 1px dashed #ccc; display: flex; align-items: center; justify-content: center; color: #999;\">Logo</div>
                {{/company.logo}}
            </div>
            <div class=\"company-info\">
                <h1>{{company.name}}</h1>
                <p>{{company.address}}</p>
                <p>Email: {{company.email}}</p>
                {{#company.pib}}<p>PIB: {{company.pib}} | Matični broj: {{company.maticni_broj}}</p>{{/company.pib}}
                {{#company.iban}}<p>Tekući račun: {{company.iban}}</p>{{/company.iban}}
            </div>
            <div class=\"client-info\">
                <p><strong>{{client.name}}</strong></p>
                <p>{{client.address}}</p>
                <p>Email: {{client.email}}</p>
                <p>{{client.payment_details}}</p>
            </div>
        </div>

        <div class=\"details\">
            <div>
                {{#invoice-id}}
                <p><strong>Račun br.:</strong> {{invoice-id}}{{#agreement}} na osnovu ugovora {{agreement}}{{/agreement}}</p>
                {{/invoice-id}}
                {{^invoice-id}}
                <p><strong>Račun br.:</strong> Draft{{#agreement}} na osnovu ugovora {{agreement}}{{/agreement}}</p>
                {{/invoice-id}}
                <p><strong>Period:</strong> {{start}} - {{end}}</p>
                <p><strong>Datum izdavanja:</strong> {{generated}}</p>
                {{#due_date}}<p><strong>Rok dospeća:</strong> {{due_date}}</p>{{/due_date}}
            </div>
            <div>
                {{#exchange_rate}}
                <p><strong>Devizni kurs:</strong> 1 EUR = {{exchange_rate}} RSD</p>
                {{/exchange_rate}}
                <p><strong>Mesto izdavanja:</strong> {{company.address}}</p>
                <p><strong>Status:</strong> {{state}}</p>
            </div>
        </div>

        {{#services}}
        <table>
            <thead>
                <tr>
                    <th>Service Description</th>
                    <th>Hours</th>
                    <th>Rate ({{client.currency}})</th>
                    <th>Amount ({{client.currency}})</th>
                </tr>
            </thead>
            <tbody>
                {{#services}}
                <tr>
                    <td>{{description}}</td>
                    <td>{{hours}}</td>
                    <td>{{rate}}</td>
                    <td>{{amount}}</td>
                </tr>
                {{/services}}
                {{^services}}
                <tr>
                    <td colspan=\"4\">No services recorded for this period</td>
                </tr>
                {{/services}}
                {{#tax-rate}}
                <tr>
                    <td colspan=\"3\"><strong>Subtotal</strong></td>
                    <td><strong>{{subtotal}} {{client.currency}}</strong></td>
                </tr>
                <tr>
                    <td colspan=\"3\"><strong>Tax ({{tax-rate}}%)</strong></td>
                    <td><strong>{{tax-amount}} {{client.currency}}</strong></td>
                </tr>
                {{/tax-rate}}
                <tr class=\"total\">
                    <td colspan=\"3\"><strong>Total</strong></td>
                    <td><strong>{{total}} {{client.currency}}</strong></td>
                </tr>
            </tbody>
        </table>
        {{/services}}

        {{#tasks}}
        <table>
            <thead>
                <tr>
                    <th>Date</th>
                    <th>Task Description</th>
                    <th>Hours</th>
                    <th>Rate ({{client.currency}})</th>
                    <th>Amount ({{client.currency}})</th>
                </tr>
            </thead>
            <tbody>
                {{#tasks}}
                <tr>
                    <td>{{date}}</td>
                    <td>{{description}}</td>
                    <td>{{hours}}</td>
                    <td>{{rate}}</td>
                    <td>{{amount}}</td>
                </tr>
                {{/tasks}}
                {{^tasks}}
                <tr>
                    <td colspan=\"5\">No tasks recorded for this period</td>
                </tr>
                {{/tasks}}
                {{#tax-rate}}
                <tr>
                    <td colspan=\"4\"><strong>Subtotal</strong></td>
                    <td><strong>{{subtotal}} {{client.currency}}</strong></td>
                </tr>
                <tr>
                    <td colspan=\"4\"><strong>Tax ({{tax-rate}}%)</strong></td>
                    <td><strong>{{tax-amount}} {{client.currency}}</strong></td>
                </tr>
                {{/tax-rate}}
                <tr class=\"total\">
                    <td colspan=\"4\"><strong>Total</strong></td>
                    <td><strong>{{total}} {{client.currency}}</strong></td>
                </tr>
            </tbody>
        </table>
        {{/tasks}}

        <div class=\"total\">
            {{#client.default_rate}}<p><strong>Rate:</strong> {{client.currency}}{{client.default_rate}}/hour</p>{{/client.default_rate}}
            <p><strong>Total Amount:</strong> {{total}} {{client.currency}}</p>
            {{#total_rsd}}<p><strong>Total Amount (RSD):</strong> {{total_rsd}} RSD</p>{{/total_rsd}}
        </div>

        {{#payment_slip}}
        <div class=\"payment-slip\">
            <div class=\"row\">
                <div class=\"col uplatilac\">
                    <label>Uplatilac</label>
                    <div>{{client.name}}<br>{{client.address}}</div>
                </div>
                <div class=\"col shifra\">
                    <label>Šifra plaćanja</label>
                    <div>189</div>
                </div>
                <div class=\"col valuta\">
                    <label>Valuta</label>
                    <div>RSD</div>
                </div>
                <div class=\"col iznos\">
                    <label>Iznos</label>
                    <div>{{total_rsd}}</div>
                </div>
            </div>

            <div class=\"row\">
                <div class=\"col svrha\">
                    <label>Svrha uplate</label>
                    <div>Račun {{invoice-id}} za {{period}}</div>
                </div>
                <div class=\"col racun\">
                    <label>Račun primaoca</label>
                    <div>{{company.iban}}</div>
                </div>
            </div>

            <div class=\"row\">
                <div class=\"col primalac\">
                    <label>Primalac</label>
                    <div>{{company.name}}<br>{{company.address}}</div>
                </div>
                <div class=\"col model\">
                    <label>Model</label>
                    <div>{{company.model}}</div>
                </div>
                <div class=\"col poziv\">
                    <label>Poziv na broj</label>
                    <div>{{company.poziv_base}}-{{period}}-{{invoice-id}}</div>
                </div>
            </div>

            <div class=\"row bottom\">
                <div class=\"col potpis\">
                    <label>Datum i potpis nalogodavca:</label>
                    <div class=\"potpis-line\"></div>
                    <em>Račun važi bez pečata i potpisa</em>
                </div>
                <div class=\"col qr\">
                    <label>NBS IPS QR</label>
                    {{#qr_code}}
                    <img src=\"data:image/png;base64,{{qr_code}}\" alt=\"QR kod\">
                    {{/qr_code}}
                    {{^qr_code}}
                    <div style=\"width: 80px; height: 80px; border: 1px dashed #ccc; display: flex; align-items: center; justify-content: center; font-size: 10px;\">QR</div>
                    {{/qr_code}}
                </div>
            </div>
        </div>
        {{/payment_slip}}

        <div class=\"footer\">
            <p>Generated on {{generated}} | Thank you for your business!</p>
        </div>
    </div>
</body>
</html>"
  "Enhanced HTML template with Serbian invoice format support."
  :type 'string
  :group 'ip-invoice)

(defcustom ip-invoice-default-template
  "<!DOCTYPE html>
<html>
<head>
  <title>Invoice {{invoice-id}}</title>
  <style>
    table { border-collapse: collapse; width: 100%; }
    td, th { border: 1px solid black; padding: 8px; text-align: left; }
    body { font-family: Arial, sans-serif; }
  </style>
</head>
<body>
  <h1>Invoice for {{client.name}}</h1>
  <p>Period: {{start}} to {{end}}</p>
  <p>State: {{state}}</p>
  {{#invoice-id}}<p>Invoice ID: {{invoice-id}}</p>{{/invoice-id}}
  {{^invoice-id}}<p>Invoice ID: Draft</p>{{/invoice-id}}
  <p>Client Address: {{client.address}}</p>
  <p>Client Email: {{client.email}}</p>
  <p>Payment Details: {{client.payment_details}}</p>
  <table>
    <tr><th>Service</th><th>Hours</th><th>Rate</th><th>Amount</th></tr>
    {{#services}}
    <tr><td>{{description}}</td><td>{{hours}}</td><td>{{rate}}</td><td>{{amount}}</td></tr>
    {{/services}}
    {{^services}}
    <tr><td colspan=\"4\">No services recorded for this period</td></tr>
    {{/services}}
    {{#tax-rate}}
    <tr><td colspan=\"3\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
    <tr><td colspan=\"3\">Tax ({{tax-rate}}%)</td><td>{{tax-amount}} {{client.currency}}</td></tr>
    {{/tax-rate}}
    {{^tax-rate}}
    <tr><td colspan=\"3\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
    {{/tax-rate}}
    <tr><td colspan=\"3\">Total</td><td>{{total}} {{client.currency}}</td></tr>
  </table>
</body>
</html>"
  "Default HTML template for service-based invoices."
  :type 'string
  :group 'ip-invoice)

(defcustom ip-invoice-task-template
  "<!DOCTYPE html>
<html>
<head>
  <title>Invoice {{invoice-id}}</title>
  <style>
    table { border-collapse: collapse; width: 100%; }
    td, th { border: 1px solid black; padding: 8px; text-align: left; }
    body { font-family: Arial, sans-serif; }
    .task-date { background-color: #f0f0f0; font-weight: bold; }
  </style>
</head>
<body>
  <h1>Invoice for {{client.name}}</h1>
  <p>Period: {{start}} to {{end}}</p>
  <p>State: {{state}}</p>
  {{#invoice-id}}<p>Invoice ID: {{invoice-id}}</p>{{/invoice-id}}
  {{^invoice-id}}<p>Invoice ID: Draft</p>{{/invoice-id}}
  <p>Client Address: {{client.address}}</p>
  <p>Client Email: {{client.email}}</p>
  <p>Payment Details: {{client.payment_details}}</p>
  <table>
    <tr><th>Date</th><th>Task</th><th>Hours</th><th>Rate</th><th>Amount</th></tr>
    {{#tasks}}
    <tr><td>{{date}}</td><td>{{description}}</td><td>{{hours}}</td><td>{{rate}}</td><td>{{amount}}</td></tr>
    {{/tasks}}
    {{^tasks}}
    <tr><td colspan=\"5\">No tasks recorded for this period</td></tr>
    {{/tasks}}
    {{#tax-rate}}
    <tr><td colspan=\"4\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
    <tr><td colspan=\"4\">Tax ({{tax-rate}}%)</td><td>{{tax-amount}} {{client.currency}}</td></tr>
    {{/tax-rate}}
    {{^tax-rate}}
    <tr><td colspan=\"4\">Subtotal</td><td>{{subtotal}} {{client.currency}}</td></tr>
    {{/tax-rate}}
    <tr><td colspan=\"4\">Total</td><td>{{total}} {{client.currency}}</td></tr>
  </table>
</body>
</html>"
  "HTML template for task-based invoices."
  :type 'string
  :group 'ip-invoice)

;;; Utility Functions
(defun ip-invoice--parse-timestamp (ts)
  "Convert org-mode timestamp TS to YYYY-MM-DD string."
  (when (and ts (eq (org-element-type ts) 'timestamp))
    (condition-case nil
        (format-time-string "%Y-%m-%d"
                            (org-time-string-to-time
                             (org-element-property :raw-value ts)))
      (error
       (ip-debug-log 'error 'invoice "Invalid timestamp: %S" ts)
       nil))))

(defun ip-invoice--task-hours (task)
  "Calculate total clocked hours for TASK."
  (let ((total-seconds 0))
    (org-element-map task 'clock
      (lambda (cl)
        (when-let ((duration (org-element-property :duration cl)))
          (condition-case nil
              (let* ((parts (split-string duration ":"))
                     (hours (string-to-number (car parts)))
                     (minutes (string-to-number (cadr parts))))
                (setq total-seconds (+ total-seconds (* hours 3600) (* minutes 60))))
            (error
             (ip-debug-log 'error 'invoice "Invalid clock duration: %s" duration)
             nil))))
      nil nil nil t)
    (ip-debug-log 'info 'invoice "Calculated hours for task: %.2f" (/ total-seconds 3600.0))
    (/ total-seconds 3600.0)))

(defun ip-invoice--task-in-range-p (task start end)
  "Check if TASK has clock entries between START and END dates."
  (let ((start-ts (date-to-time start))
        (end-ts (date-to-time end)))
    (cl-some
     (lambda (cl)
       (when-let ((ts (org-element-property :value cl))
                  (duration (org-element-property :duration cl)))
         (condition-case nil
             (let* ((clock-start (org-time-string-to-time
                                  (org-element-property :raw-value ts)))
                    (parts (split-string duration ":"))
                    (hours (string-to-number (car parts)))
                    (minutes (string-to-number (cadr parts)))
                    (seconds (+ (* hours 3600) (* minutes 60)))
                    (clock-end (time-add clock-start seconds)))
               (ip-debug-log 'info 'invoice "Checking clock: %s to %s, range: %s to %s"
                             (format-time-string "%Y-%m-%d" clock-start)
                             (format-time-string "%Y-%m-%d" clock-end)
                             start end)
               (and (time-less-p clock-start end-ts)
                    (time-less-p start-ts clock-end)))
           (error
            (ip-debug-log 'error 'invoice "Error checking clock range: %S" cl)
            nil))))
     (org-element-map task 'clock #'identity nil nil nil t))))

(defun ip-invoice--parse-task (task)
  "Extract task information as a plist."
  (condition-case err
      (let* ((raw-title (org-element-property :raw-value task))
             (tags (org-element-property :tags task))
             (clean-title (replace-regexp-in-string ":[^:]+:" "" raw-title)) ; Remove embedded tags
             (title (string-trim clean-title))
             (client (string-trim
                      (or (cl-find-if (lambda (tag) (member tag (ip-list-client-ids))) tags)
                          (and (string-match ":maketv:" raw-title) "maketv")
                          "unknown")))
             (service (string-trim
                       (or (cl-find-if (lambda (tag) (member tag (ip-list-service-tags))) tags)
                           (and (member "documentation" tags) "documentation")
                           (and (member "infrastructure" tags) "infrastructure")
                           "general")))
             (hours (ip-invoice--task-hours task)))
        (when (string-match ":[^:]+:" raw-title)
          (ip-debug-log 'warn 'invoice "Malformed task title with embedded tags: %s" raw-title))
        (ip-debug-log 'debug 'invoice "Raw title: %S, cleaned title: %S, org-tags: %S"
                      raw-title title tags)
        (when (string= client "unknown")
          (ip-debug-log 'warning 'invoice "Client ID not found for task: %s, tags: %S, using 'unknown'" title tags))
        (ip-debug-log 'info 'invoice "Parsed task: %s, client: %s, service: %s, hours: %.2f, tags: %S"
                      title client service hours tags)
        (list :client client :service service :title title :hours hours :element task))
    (error
     (ip-debug-log 'error 'invoice "Error parsing task: %s" (error-message-string err))
     nil)))

(defun ip-invoice--load-tasks (start end)
  "Load tasks within START and END date range."
  (ip-debug-log 'info 'invoice "Loading tasks from %s to %s" start end)
  (unless (and ip-tasks-file (file-exists-p (ip--get-full-path ip-tasks-file)))
    (ip-debug-log 'error 'invoice "Tasks file not found: %s" (ip--get-full-path ip-tasks-file))
    (error "Tasks file not found: %s" (ip--get-full-path ip-tasks-file)))
  (let ((ast (ip--load-org-file ip-tasks-file))
        (count 0))
    (prog1
        (cl-loop for hl in (org-element-map ast 'headline #'identity nil nil nil t)
                 when (ip-invoice--task-in-range-p hl start end)
                 do (cl-incf count)
                 collect (ip-invoice--parse-task hl))
      (ip-debug-log 'success 'invoice "Loaded %d tasks" count))))

(defun ip-invoice--generate-invoice-id ()
  "Generate a unique invoice ID."
  (unless (file-directory-p ip-invoice-final-dir)
    (make-directory ip-invoice-final-dir t)
    (ip-debug-log 'info 'invoice "Created directory: %s" ip-invoice-final-dir))
  (let* ((files (directory-files ip-invoice-final-dir nil "^invoice-.*\\.html$"))
         (nums (mapcar (lambda (f)
                         (or (and (string-match "^invoice-INV-2025-\\([0-9]+\\)" f)
                                  (string-to-number (match-string 1 f)))
                             0))
                       files))
         (next (1+ (apply #'max 0 nums))))
    (ip-debug-log 'info 'invoice "Generated invoice ID: INV-2025-%04d" next)
    (format "INV-2025-%04d" next)))

(defun ip-invoice--get-task-clock-entries (task start end)
  "Get clock entries for TASK within START and END date range."
  (let ((start-ts (date-to-time start))
        (end-ts (date-to-time end)))
    (org-element-map task 'clock
      (lambda (cl)
        (when-let ((ts (org-element-property :value cl))
                   (duration (org-element-property :duration cl)))
          (condition-case nil
              (let* ((clock-start (org-time-string-to-time
                                   (org-element-property :raw-value ts)))
                     (parts (split-string duration ":"))
                     (hours (string-to-number (car parts)))
                     (minutes (string-to-number (cadr parts)))
                     (seconds (+ (* hours 3600) (* minutes 60)))
                     (clock-end (time-add clock-start seconds)))
                (when (and (time-less-p clock-start end-ts)
                           (time-less-p start-ts clock-end))
                  (ip-debug-log 'info 'invoice "Clock entry: %s, hours: %.2f"
                                (format-time-string "%Y-%m-%d" clock-start)
                                (/ (+ (* hours 3600) (* minutes 60)) 3600.0))
                  (list :date (format-time-string "%Y-%m-%d" clock-start)
                        :hours (/ (+ (* hours 3600) (* minutes 60)) 3600.0))))
            (error
             (ip-debug-log 'error 'invoice "Error parsing clock: %S" cl)
             nil))))
      nil nil nil t)))

;;; Core Functions
(defun ip-invoice-generate-data (client-id start end &optional state invoice-type)
  "Generate invoice data for CLIENT-ID from START to END.
STATE is \\='draft or \\='final. INVOICE-TYPE is \\='service or \\='task."
  (ip-debug-log 'info 'invoice "Generating invoice for %s (%s to %s, type: %s)"
                client-id start end (or invoice-type ip-invoice-type))
  (ip-refresh-cache)
  (let* ((client-id (string-trim client-id))
         (client (or (ip-get-client-by-id client-id)
                     (progn
                       (ip-debug-log 'error 'invoice "Unknown client ID: %s" client-id)
                       (error "Unknown client ID: %s" client-id))))
         (client-name (or (plist-get client :NAME) client-id))
         (currency (or (plist-get client :CURRENCY) "EUR"))
         (tax-rate (or (string-to-number (or (plist-get client :TAX_RATE) "0")) 0.0))
         (default-rate (string-to-number (or (plist-get client :DEFAULT_RATE) "0")))
         (invoice-type (or invoice-type ip-invoice-type))
         (invoice-id (if (eq state 'final)
                         (ip-invoice--generate-invoice-id)
                       (format "DRAFT-%s-%s" client-id (format-time-string "%Y%m%d")))))
    (ip-debug-log 'info 'invoice "Client: %s, currency: %s, tax-rate: %.2f, default-rate: %.2f, invoice-id: %s"
                  client-name currency tax-rate default-rate invoice-id)
    (cond
     ((eq invoice-type 'service)
      (ip-debug-log 'info 'invoice "Generating service-based invoice")
      (let* ((tasks (ip-invoice--load-tasks start end))
             (filtered (cl-remove-if-not
                        (lambda (task)
                          (let ((valid (and task (string= (string-trim (plist-get task :client)) client-id)
                                            (> (plist-get task :hours) 0))))
                            (ip-debug-log 'info 'invoice "Filtering task: %s, client: %s, hours: %.2f, valid: %S"
                                          (plist-get task :title) (plist-get task :client)
                                          (plist-get task :hours) valid)
                            valid))
                        tasks))
             (grouped (make-hash-table :test 'equal))
             (services ())
             (subtotal 0.0))
        (ip-debug-log 'info 'invoice "Found %d tasks for %s" (length filtered) client-id)
        ;; Group by service
        (dolist (task filtered)
          (when-let ((hours (plist-get task :hours))
                     (service (string-trim (plist-get task :service))))
            (puthash service
                     (+ (gethash service grouped 0.0) hours)
                     grouped)))
        ;; Calculate service amounts
        (maphash
         (lambda (svc hours)
           (let* ((svc-data (ip-get-client-service client-id svc))
                  (rate (string-to-number (or (plist-get svc-data :RATE)
                                              (plist-get client :DEFAULT_RATE)
                                              "0")))
                  (amount (* rate hours)))
             (ip-debug-log 'info 'invoice "Service %s: %.2f hours @ %.2f = %.2f"
                           svc hours rate amount)
             (push (list :description (or (plist-get svc-data :description) svc)
                         :hours (format "%.2f" hours)
                         :rate (format "%.2f" rate)
                         :amount (format "%.2f" amount)
                         :taxable (plist-get svc-data :TAXABLE))
                   services)
             (setq subtotal (+ subtotal amount))))
         grouped)
        (let ((tax-amount (* subtotal (/ tax-rate 100.0))))
          (list :client (plist-put client :NAME client-name)
                :start start
                :end end
                :state (or state 'draft)
                :invoice-id invoice-id
                :type 'service
                :currency currency
                :services (nreverse services)
                :subtotal (format "%.2f" subtotal)
                :tax-rate tax-rate
                :tax-amount (format "%.2f" tax-amount)
                :total (format "%.2f" (+ subtotal tax-amount))))))
     ((eq invoice-type 'task)
      (ip-debug-log 'info 'invoice "Generating task-based invoice")
      (let* ((tasks (ip-invoice--load-tasks start end))
             (filtered (cl-remove-if-not
                        (lambda (task)
                          (let ((valid (and task (string= (string-trim (plist-get task :client)) client-id)
                                            (> (plist-get task :hours) 0))))
                            (ip-debug-log 'info 'invoice "Filtering task: %s, client: %s, hours: %.2f, valid: %S"
                                          (plist-get task :title) (plist-get task :client)
                                          (plist-get task :hours) valid)
                            valid))
                        tasks))
             (task-entries ())
             (subtotal 0.0))
        (ip-debug-log 'info 'invoice "Found %d tasks for %s" (length filtered) client-id)
        ;; Process task clock entries
        (dolist (task filtered)
          (when-let ((service (string-trim (plist-get task :service)))
                     (title (plist-get task :title))
                     (element (plist-get task :element))
                     (svc-data (ip-get-client-service client-id service))
                     (rate (string-to-number (or (plist-get svc-data :RATE)
                                                 (plist-get client :DEFAULT_RATE)
                                                 "0"))))
            (dolist (entry (ip-invoice--get-task-clock-entries element start end))
              (let ((hours (plist-get entry :hours))
                    (date (plist-get entry :date)))
                (ip-debug-log 'info 'invoice "Task entry: %s, %s, %.2f hours"
                              date title hours)
                (push (list :date date
                            :description (encode-coding-string title 'utf-8)
                            :hours (format "%.2f" hours)
                            :rate (format "%.2f" rate)
                            :amount (format "%.2f" (* rate hours)))
                      task-entries)
                (setq subtotal (+ subtotal (* rate hours)))))))
        (let ((tax-amount (* subtotal (/ tax-rate 100.0))))
          (list :client (plist-put client :NAME client-name)
                :start start
                :end end
                :state (or state 'draft)
                :invoice-id invoice-id
                :type 'task
                :currency currency
                :tasks (sort task-entries
                             (lambda (a b) (string< (plist-get a :date) (plist-get b :date))))
                :subtotal (format "%.2f" subtotal)
                :tax-rate tax-rate
                :tax-amount (format "%.2f" tax-amount)
                :total (format "%.2f" (+ subtotal tax-amount))))))
     (t
      (ip-debug-log 'error 'invoice "Unknown invoice type: %s" invoice-type)
      (error "Unknown invoice type: %s" invoice-type)))))

(defun ip-invoice--convert-plist-to-mustache-data (plist)
  "Convert a PLIST with keyword keys to a Mustache-compatible alist."
  (let (result)
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (push (cons (substring (symbol-name key) 1) ; Remove leading colon
                    (cond
                     ((and (listp value) (not (null value)) (keywordp (car value)))
                      (ip-invoice--convert-plist-to-mustache-data value))
                     ((and (listp value) (listp (car value)))
                      (mapcar #'ip-invoice--convert-plist-to-mustache-data value))
                     (t value)))
              result)
        (setq plist (cddr plist))))
    (nreverse result)))

(defun ip-invoice--generate-html (invoice output-file)
  "Generate HTML invoice from INVOICE data to OUTPUT-FILE."
  (condition-case err
      (progn
        ;; Validate inputs
        (unless (and (listp invoice) (plist-get invoice :invoice-id))
          (error "Invalid invoice data: missing :invoice-id"))
        (unless (file-writable-p output-file)
          (error "Output file %s is not writable" output-file))
        (ip-debug-log 'info 'invoice "Generating HTML invoice: %s" output-file)
        (ip-debug-log 'debug 'invoice "Services count: %d" (length (or (plist-get invoice :services) ())))
        (ip-debug-log 'debug 'invoice "Tasks count: %d" (length (or (plist-get invoice :tasks) ())))
        
        ;; Construct data for Mustache template
        (let* ((template (pcase ip-invoice-template-choice
                           ('file (if (and ip-invoice-template-file
                                           (file-readable-p ip-invoice-template-file))
                                      (with-temp-buffer
                                        (insert-file-contents ip-invoice-template-file)
                                        (buffer-string))
                                    (ip-debug-log 'warn 'invoice "Template file %s not readable, using fallback"
                                                  ip-invoice-template-file)
                                    ip-invoice-enhanced-template))
                           ('default ip-invoice-default-template)
                           (_ ip-invoice-enhanced-template)))
               (company-data (or (ip-get-company-info)
                                 (progn
                                   (ip-debug-log 'warn 'invoice "Company data not found, using defaults")
                                   (list :NAME "Default Company" :ADDRESS "N/A" :EMAIL "N/A"))))
               (client-data (or (plist-get invoice :client)
                                (error "Missing client data in invoice")))
               (base-data (list
                           :invoice-id (or (plist-get invoice :invoice-id) "")
                           :client (list
                                    :name (or (plist-get client-data :NAME) "Unknown Client")
                                    :address (or (plist-get client-data :ADDRESS) "N/A")
                                    :email (or (plist-get client-data :EMAIL) "N/A")
                                    :payment_details (or (plist-get client-data :PAYMENT_DETAILS) "N/A")
                                    :currency (or (plist-get invoice :currency) "EUR")
                                    :default_rate (or (plist-get client-data :DEFAULT_RATE) "0"))
                           :company (list
                                     :name (or (plist-get company-data :NAME) "My Company")
                                     :address (or (plist-get company-data :ADDRESS) "N/A")
                                     :email (or (plist-get company-data :EMAIL) "N/A")
                                     :pib (or (plist-get company-data :TAX_ID) "N/A")
                                     :iban (or (plist-get company-data :IBAN) "N/A")
                                     :model (or (plist-get company-data :MODEL) "97")
                                     :poziv_base (or (plist-get company-data :POZIV_BASE) "123-456")
                                     :logo (or (plist-get company-data :LOGO) nil))
                           :start (or (plist-get invoice :start) "N/A")
                           :end (or (plist-get invoice :end) "N/A")
                           :state (or (symbol-name (plist-get invoice :state)) "draft")
                           :generated (format-time-string "%Y-%m-%d")
                           :due_date (or (plist-get invoice :due_date)
                                         (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 14))))
                           :exchange_rate (when ip-invoice-exchange-rate
                                            (format "%.2f" ip-invoice-exchange-rate))
                           :payment_slip ip-invoice-include-payment-slip))
               (services-data (when (eq (plist-get invoice :type) 'service)
                                ;; Validate and map services
                                (mapc (lambda (svc)
                                        (unless (listp svc)
                                          (error "Invalid service data: %S" svc)))
                                      (or (plist-get invoice :services) ()))
                                (mapcar (lambda (svc)
                                          (list
                                           :description (or (plist-get svc :description) "")
                                           :hours (or (plist-get svc :hours) "0.00")
                                           :rate (or (plist-get svc :rate) "0.00")
                                           :amount (or (plist-get svc :amount) "0.00")))
                                        (or (plist-get invoice :services) ()))))
               (tasks-data (when (eq (plist-get invoice :type) 'task)
                             ;; Validate and map tasks
                             (mapc (lambda (task)
                                     (unless (listp task)
                                       (error "Invalid task data: %S" task)))
                                   (or (plist-get invoice :tasks) ()))
                             (mapcar (lambda (task)
                                       (list
                                        :date (or (plist-get task :date) "")
                                        :description (or (plist-get task :description) "")
                                        :hours (or (plist-get task :hours) "0.00")
                                        :rate (or (plist-get task :rate) "0.00")
                                        :amount (or (plist-get task :amount) "0.00")))
                                     (or (plist-get invoice :tasks) ()))))
               (tax-data (when (plist-get invoice :tax-rate)
                           (list
                            :tax-rate (plist-get invoice :tax-rate)
                            :tax-amount (or (plist-get invoice :tax-amount) "0.00"))))
               (total-data (list
                            :subtotal (or (plist-get invoice :subtotal) "0.00")
                            :total (or (plist-get invoice :total) "0.00")
                            :total_rsd (when (and ip-invoice-exchange-rate
                                                  (plist-get invoice :total))
                                         (format "%.2f" (* (string-to-number (plist-get invoice :total))
                                                           ip-invoice-exchange-rate)))))
               (data (append base-data
                             (when services-data (list :services services-data))
                             (when tasks-data (list :tasks tasks-data))
                             (when tax-data (list :tax-rate (plist-get tax-data :tax-rate)
                                                  :tax-amount (plist-get tax-data :tax-amount)))
                             total-data)))
          (ip-debug-log 'debug 'invoice "Invoice data structure: %S" data)
          ;; Write to file
          (with-temp-file output-file
            (set-buffer-file-coding-system 'utf-8)
            (insert (mustache-render template data)))
          (ip-debug-log 'info 'invoice "Successfully generated HTML invoice: %s" output-file)))
    (error
     (ip-debug-log 'error 'invoice "Failed to generate HTML: %s" (error-message-string err))
     (error "Failed to generate HTML invoice: %s" (error-message-string err)))))


;;;###autoload
(defun ip-invoice-select-template ()
  "Select invoice template to use."
  (interactive)
  (let ((choice (completing-read "Select template: "
                                 '("default" "enhanced" "file")
                                 nil t)))
    (setq ip-invoice-template-choice (intern choice))
    (message "Selected template: %s" choice)))

;;;###autoload
(defun ip-invoice-preview-text (client-id start end &optional state invoice-type)
  "Display a textual preview of the invoice for CLIENT-ID from START to END."
  (interactive
   (list
    (completing-read "Client ID: " (ip-list-client-ids))
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")
    nil
    (intern (completing-read "Invoice type: " '("service" "task") nil t))))
  (ip-debug-log 'info 'invoice "Generating text preview for %s" client-id)
  (let* ((invoice (ip-invoice-generate-data client-id start end state invoice-type))
         (buf (get-buffer-create "*Invoice Preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Invoice for %s\n" (or (plist-get (plist-get invoice :client) :NAME) client-id)))
        (insert (format "Period: %s to %s\n" start end))
        (insert (format "State: %s\n" (plist-get invoice :state)))
        (insert (format "Type: %s\n" (plist-get invoice :type)))
        (when-let ((id (plist-get invoice :invoice-id)))
          (insert (format "Invoice ID: %s\n" id)))
        (cond
         ((eq (plist-get invoice :type) 'service)
          (insert "\nServices:\n")
          (if (plist-get invoice :services)
              (dolist (svc (plist-get invoice :services))
                (insert (format "- %s: %s hours @ %s/%s = %s %s\n"
                                (plist-get svc :description)
                                (plist-get svc :hours)
                                (plist-get svc :rate)
                                (plist-get invoice :currency)
                                (plist-get svc :amount)
                                (plist-get invoice :currency))))
            (insert "- No services found\n")))
         ((eq (plist-get invoice :type) 'task)
          (insert "\nTasks:\n")
          (if (plist-get invoice :tasks)
              (dolist (task (plist-get invoice :tasks))
                (insert (format "- %s: %s (%s hours @ %s/%s = %s %s)\n"
                                (plist-get task :date)
                                (plist-get task :description)
                                (plist-get task :hours)
                                (plist-get task :rate)
                                (plist-get invoice :currency)
                                (plist-get task :amount)
                                (plist-get invoice :currency))))
            (insert "- No tasks found\n"))))
        (when (plist-get invoice :tax-rate)
          (insert (format "\nSubtotal: %s %s\n"
                          (plist-get invoice :subtotal)
                          (plist-get invoice :currency)))
          (insert (format "Tax (%.2f%%): %s %s\n"
                          (plist-get invoice :tax-rate)
                          (plist-get invoice :tax-amount)
                          (plist-get invoice :currency))))
        (insert (format "Total: %s %s\n"
                        (plist-get invoice :total)
                        (plist-get invoice :currency))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf)
    (ip-debug-log 'success 'invoice "Text preview generated for %s" client-id)))

;;;###autoload
(defun ip-invoice-create (client-id start end &optional final invoice-type)
  "Create invoice for CLIENT-ID from START to END.
If FINAL is non-nil, generate a final invoice with a unique ID.
INVOICE-TYPE is \\='service or \\='task."
  (interactive
   (list
    (completing-read "Client ID: " (ip-list-client-ids))
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")
    (y-or-n-p "Final invoice? ")
    (intern (completing-read "Invoice type: " '("service" "task") nil t))))
  (condition-case err
      (let* ((state (if final 'final 'draft))
             (invoice-type (or invoice-type ip-invoice-type))
             (invoice (ip-invoice-generate-data client-id start end state invoice-type))
             (output-dir (if final ip-invoice-final-dir ip-invoice-draft-dir))
             (type-suffix (if (eq invoice-type 'task) "-tasks" ""))
             (filename (format "%sinvoice-%s%s.html" output-dir
                               (or (plist-get invoice :invoice-id)
                                   (format "%s-%s" client-id (format-time-string "%Y%m%d")))
                               type-suffix)))
        (ip-debug-log 'info 'invoice "Creating %s invoice for %s (%s to %s)"
                      (if final "final" "draft") client-id start end)
        (ip-debug-log 'debug 'invoice "Invoice data: %S" invoice)
        (unless (file-directory-p output-dir)
          (make-directory output-dir t)
          (ip-debug-log 'info 'invoice "Created directory: %s" output-dir))
        (ip-invoice--generate-html invoice filename)
        (message "Invoice created: %s" filename)
        filename) ; Return the filename for further processing
    (error
     (ip-debug-log 'error 'invoice "Failed to create invoice for %s: %s"
                   client-id (error-message-string err))
     (message "Error creating invoice for %s: %s" client-id (error-message-string err))
     nil))) ; Return nil to indicate failure

;;;###autoload
(defun ip-invoice-create-both (client-id start end &optional final)
  "Create both service and task invoices for CLIENT-ID from START to END."
  (interactive
   (list
    (completing-read "Client ID: " (ip-list-client-ids))
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")
    (y-or-n-p "Final invoices? ")))
  (ip-debug-log 'info 'invoice "Creating both invoices for %s (%s)"
                client-id (if final "final" "draft"))
  (ip-invoice-create client-id start end final 'service)
  (ip-invoice-create client-id start end final 'task)
  (message "Service and task invoices created for %s" client-id))

(provide 'ip-invoice)
;;; ip-invoice.el ends here
