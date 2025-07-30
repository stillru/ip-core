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


(defgroup ip-invoice nil "Invoice generator." :group 'ip-core)

(defcustom ip-invoice-template-file "~/Documents/ORG/templates/invoice-template.html"
  "Path to the enhanced HTML invoice template." :type 'file :group 'ip-invoice)

(defcustom ip-invoice-draft-dir (expand-file-name "invoices/draft/" ip-org-directory)
  "Draft invoices directory." :type 'directory :group 'ip-invoice)

(defcustom ip-invoice-final-dir (expand-file-name "invoices/final/" ip-org-directory)
  "Final invoices directory." :type 'directory :group 'ip-invoice)

(defcustom ip-invoice-include-payment-slip t
  "Include Serbian payment slip with QR code." :type 'boolean :group 'ip-invoice)

(defun ip-invoice--get-exchange-rate ()
  "Return fixed EUR/RSD exchange rate (stub)."
  117.85)

(defun ip-invoice--generate-invoice-id ()
  "Generate a unique invoice ID based on current time."
  (format-time-string "INV-%Y%m%d-%H%M%S"))

(defun ip-invoice--get-clock-entries (start end client-id)
  "Get clock entries for CLIENT-ID between START and END."
  (let ((entries '())
        (start-ts (date-to-time start))
        (end-ts (date-to-time end)))
    (org-map-entries
     (lambda ()
       (when (string= (org-entry-get nil "CLIENT") client-id)
         (let ((desc (org-get-heading t t))
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
                       (push (list :date date
                                   :description (encode-coding-string desc 'utf-8)
                                   :hours (format "%.2f" hours-float)
                                   :rate (format "%.2f" rate)
                                   :amount (format "%.2f" amount))
                             entries))))))))))
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
    ;; В реальной версии здесь будет вызов NBS API
    ;; Пока просто возвращаем заглушку
    (ip-debug-log 'info 'invoice "Mock QR code for: %s" qr-data)
    "iVBORw0KGgoAAAANSUhEUgAAAMgAAADIAQAAAACFI9sAAAAH0lEQVR42u3BAQ0AAADCoPdPbQ43oAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADwDcaiAAFXqzYQAAAAAElFTkSuQmCC"))

(defun ip-invoice-generate-data (client-id start end &optional state invoice-type)
  "Generate invoice data for CLIENT-ID from START to END.
Returns a plist with :tasks-plain and :tasks-aggregated."
  (ip-debug-log 'info 'invoice "Generating invoice for %s (%s to %s)" client-id start end)
  (let* ((client-id (string-trim client-id))
         (client (or (ip-get-client-by-id client-id)
                     (error "Unknown client ID: %s" client-id)))
         (client-name (or (plist-get client :NAME) client-id))
         (currency (or (plist-get client :CURRENCY) "EUR"))
         (tax-rate (or (string-to-number (or (plist-get client :TAX_RATE) "0")) 0.0))
         (default-rate (string-to-number (or (plist-get client :DEFAULT_RATE) "0")))
         (invoice-id (if (eq state 'final)
                         (ip-invoice--generate-invoice-id)
                       (format "DRAFT-%s-%s" client-id (format-time-string "%Y%m%d"))))
         (generated (format-time-string "%Y-%m-%d"))
         (due-date (format-time-string "%Y-%m-%d" (time-add (date-to-time end) (days-to-time 13))))
         (company (ip-get-company-info))
         (exchange-rate 117.85)  ; заглушка
         (tasks-plain ())
         (tasks-aggregated (make-hash-table :test 'equal))
         (subtotal 0.0))
    ;; Загружаем и обрабатываем записи
    (let ((entries (ip-invoice--get-clock-entries start end client-id)))
      (dolist (entry entries)
        (let* ((desc (plist-get entry :description))
               (hours (string-to-number (plist-get entry :hours)))
               (rate (string-to-number (plist-get entry :rate)))
               (amount (* hours rate)))
          ;; Добавляем в plain
          (push entry tasks-plain)
          ;; Агрегируем
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
    ;; Преобразуем hash-table в список
    (let ((aggregated-list (maphash (lambda (desc data)
                                      (list :description desc
                                            :total_hours (format "%.2f" (plist-get data :total_hours))
                                            :amount (format "%.2f" (plist-get data :amount))))
                                    tasks-aggregated)))
      ;; Сортируем
      (setq tasks-plain (sort tasks-plain (lambda (a b) (string< (plist-get a :date) (plist-get b :date)))))
      (setq aggregated-list (sort aggregated-list (lambda (a b) (string< (plist-get a :description) (plist-get b :description)))))
      ;; Формируем результат
      (let* ((tax-amount (* subtotal (/ tax-rate 100.0)))
             (total-amount-eur (+ subtotal tax-amount))
             (total-amount-rsd (* total-amount-eur exchange-rate)))
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
              :company company)))))

(defun ip-invoice--last-day-of-month (year month)
  "Return last day of MONTH in YEAR as string YYYY-MM-DD."
  (let* ((time (encode-time 0 0 0 1 month year))
         (next-month (time-add time (days-to-time 32)))
         (next-month-num (string-to-number (format-time-string "%m" next-month))))
    (if (and (= year (string-to-number (format-time-string "%Y" next-month)))
             (= (mod (1- (+ month 1)) 12) (1- next-month-num)))
        (format-time-string "%Y-%m-%d" (time-subtract next-month (seconds-to-time 86400)))
      (format "%04d-%02d-%02d" year month (cadr (calendar-last-day-of-month month year))))))

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
(defun ip-invoice-generate (client-id start end &optional state invoice-type)
  "Generate an invoice and save to file."
  (interactive (list (completing-read "Client ID: " (ip-list-client-ids))))
  (let* ((invoice (ip-invoice-generate-data client-id start end state invoice-type))
         (output-dir (if (eq state 'final) ip-invoice-final-dir ip-invoice-draft-dir))
         (output-file (expand-file-name (format "%s-%s.html" client-id (plist-get invoice :invoice-id)) output-dir)))
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    (ip-invoice--generate-html invoice output-file)
    (browse-url (concat "file://" output-file))))


(provide 'ip-invoice)
;;; ip-invoice.el ends here
