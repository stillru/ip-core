;;; ip-invoice.el --- Generate invoice data from org-mode tasks -*- lexical-binding: t; -*-

(require 'org-element)
(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'ip-core)

(defvar ip-invoice-final-dir "~/org/ip/invoices/2025/"
  "Directory to store finalized invoices.")

(defvar ip-invoice-draft-dir "~/org/ip/invoices/drafts/"
  "Directory to store draft invoices.")

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
  (/ (float (org-element-property :duration task)) 60.0))

(defun ip--parse-task (task)
  "Extract relevant info from TASK headline."
  (let* ((props (org-element-property :properties task))
         (client (cdr (assoc-string "CLIENT" props t)))
         (service (cdr (assoc-string "SERVICE" props t)))
         (title (org-element-property :raw-value task))
         (duration (ip--task-total-hours task)))
    (list :title title :client client :service service :hours duration)))

(defun ip--find-client-service-rate (client-id service)
  "Find hourly rate for SERVICE under CLIENT-ID. Fallback to client-level RATE."
  (let* ((clients (ip-get-clients))
         (client (cl-find-if (lambda (c)
                               (string= (plist-get c :ID) client-id))
                             clients)))
    (or (plist-get (cl-find-if (lambda (s)
                                 (string= (plist-get s :SERVICE) service))
                               (plist-get client :services))
                   :RATE)
        (plist-get client :RATE))))

(defun ip--load-tasks-in-range (start end)
  "Return list of tasks as plists, filtered by clock range."
  (let ((ast (ip--load-org-file ip-tasks-file)))
    (cl-loop for hl in (org-element-map ast 'headline #'identity)
             when (ip--task-in-range-p hl start end)
             collect (ip--parse-task hl))))

(defun ip-generate-invoice-data (client-id start end &optional state)
  "Generate invoice as plist for CLIENT-ID from tasks between START and END. STATE is either 'draft or 'final."
  (let* ((tasks (ip--load-tasks-in-range start end))
         (filtered (cl-remove-if-not (lambda (t)
                                       (string= (plist-get t :client) client-id))
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
                 (let* ((rate (string-to-number (or (ip--find-client-service-rate client-id svc) "0")))
                        (amount (* rate hours)))
                   (push (list :service svc :hours hours :rate rate :amount amount) services)))
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
  "Show a textual invoice buffer for CLIENT-ID from START to END. STATE is 'draft or 'final."
  (interactive "sClient ID: \nsStart date (YYYY-MM-DD): \nsEnd date (YYYY-MM-DD): ")
  (let* ((invoice (ip-generate-invoice-data client-id start end state))
         (buf (get-buffer-create "*Invoice Preview*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Invoice for %s\n" (plist-get (plist-get invoice :client) :name)))
      (insert (format "Period: %s to %s\n" start end))
      (insert (format "State: %s\n" (plist-get invoice :state)))
      (when-let ((id (plist-get invoice :invoice-id)))
        (insert (format "Invoice ID: %s\n" id)))
      (insert "\n")
      (insert (format "| %-15s | %-5s | %-7s | %-10s |\n" "Service" "Hours" "Rate" "Amount"))
      (insert (make-string 60 ?-) "\n")
      (dolist (svc (plist-get invoice :services))
        (insert (format "| %-15s | %5.2f | %7.2f | %10.2f |\n"
                        (plist-get svc :service)
                        (plist-get svc :hours)
                        (plist-get svc :rate)
                        (plist-get svc :amount))))
      (insert (make-string 60 ?-) "\n")
      (insert (format "| TOTAL%43s | %10.2f |\n" "" (plist-get invoice :total)))
      (goto-char (point-min))
      (read-only-mode 1)
      (display-buffer buf))))

(provide 'ip-invoice)
;;; ip-invoice.el ends here
