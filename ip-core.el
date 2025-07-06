;;; ip-core.el --- Core module for IP management in Org-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'org-element)
(require 'cl-lib)

(defgroup ip-core nil
  "Personal business management via Org-mode."
  :prefix "ip-"
  :group 'applications)

(defcustom ip-org-directory "~/Documents/ORG/"
  "Directory where IP-related org files are stored."
  :type 'directory
  :group 'ip-core)

(defcustom ip-company-file "company.org"
  "Filename for personal company info."
  :type 'string
  :group 'ip-core)

(defcustom ip-clients-file "clients.org"
  "Filename for client definitions."
  :type 'string
  :group 'ip-core)

(defcustom ip-tasks-file "tasks.org"
  "Filename for task tracking."
  :type 'string
  :group 'ip-core)




(defun ip--load-org-file (filename)
  "Return parsed Org AST from FILENAME in `ip-org-directory'."
  (let ((path (expand-file-name filename ip-org-directory)))
    (unless (file-exists-p path)
      (error "File %s does not exist" path))
    (with-current-buffer (find-file-noselect path)
      (org-element-parse-buffer))))

(defun ip--get-headlines (ast)
  "Extract top-level headlines from AST."
  (org-element-map ast 'headline
    (lambda (hl)
      (when (= (org-element-property :level hl) 1)
        hl))))

(defun ip--parse-properties (hl)
  "Extract :PROPERTIES: from a headline as plist."
  (let ((props (org-element-property :properties hl)))
    (if props
        (cl-loop for (k . v) in props
                 append (list (intern (concat ":" (upcase (symbol-name k)))) v))
      '())))

(defun ip-get-clients ()
  "Return list of clients as plists from `clients.org'."
  (let ((ast (ip--load-org-file ip-clients-file)))
    (cl-loop for hl in (ip--get-headlines ast)
             collect (append (list :name (org-element-property :raw-value hl))
                             (ip--parse-properties hl)))))

(defun ip-get-company-info ()
  "Return company info from `company.org' as plist (first headline)."
  (let* ((ast (ip--load-org-file ip-company-file))
         (hl (car (ip--get-headlines ast))))
    (append (list :name (org-element-property :raw-value hl))
            (ip--parse-properties hl))))

;;;###autoload
(defun ip-show-overview ()
  "Show summary of company and client list."
  (interactive)
  (let ((company (ip-get-company-info))
        (clients (ip-get-clients)))
    (with-current-buffer (get-buffer-create "*IP Overview*")
      (erase-buffer)
      (insert (format "Company: %s\n" (plist-get company :name)))
      (insert (format "Address: %s\n" (plist-get company :ADDRESS)))
      (insert (format "IBAN: %s\n\n" (plist-get company :IBAN)))
      (insert "Clients:\n")
      (dolist (c clients)
        (insert (format "- %s (%s/hr)\n"
                        (plist-get c :name)
                        (or (plist-get c :RATE) "?"))))
      (goto-char (point-min))
      (read-only-mode 1)
      (pop-to-buffer (current-buffer)))))

(provide 'ip-core)
;;; ip-core.el ends here
