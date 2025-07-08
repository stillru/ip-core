;;; ip-core.el --- Core module for IP management in Org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Core utilities for IP Manager, including client and file handling.

;;; Code:
(require 'org-element)
(require 'cl-lib)

(defgroup ip-core nil
  "Personal business management via Org-mode."
  :prefix "ip-"
  :group 'applications)

(defcustom ip-org-directory "~/org/ip/"
  "Directory where IP-related Org files (e.g., company, clients, tasks) are stored."
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
  (let ((drawer (org-element-map hl 'property-drawer #'identity)))
    (if drawer
        (cl-loop for node in (org-element-contents (car drawer))
                 when (eq (org-element-type node) 'node-property)
                 append (list (intern (concat ":" (upcase (org-element-property :key node))))
                              (org-element-property :value node)))
      '())))

(defun ip--normalize-tag (name)
  "Normalize NAME for matching with tags (lowercase, no spaces or special chars)."
  (downcase
   (replace-regexp-in-string "[^a-zA-Z0-9]" "" name)))

(defun ip-get-clients ()
  "Return list of clients as plists from `clients.org'."
  (condition-case err
      (let ((ast (ip--load-org-file ip-clients-file)))
        (cl-loop for hl in (ip--get-headlines ast)
                 collect (let* ((client-name (org-element-property :raw-value hl))
                                (client-id (ip--normalize-tag client-name))
                                (props (ip--parse-properties hl))
                                (services (org-element-map hl 'headline
                                            (lambda (sub-hl)
                                              (when (= (org-element-property :level sub-hl) 2)
                                                (let ((service-name (org-element-property :raw-value sub-hl)))
                                                  (append
                                                   (list :description service-name
                                                         :tag (ip--normalize-tag service-name))
                                                   (ip--parse-properties sub-hl)))))))
                           (append (list :NAME client-name
                                         :ID client-id)
                                   props
                                   (list :services services)))))
    (error (message "Error loading clients: %s" err) '())))

(defun ip-get-company-info ()
  "Return company info from `company.org' as plist (first headline)."
  (condition-case err
      (let* ((ast (ip--load-org-file ip-company-file))
             (hl (car (ip--get-headlines ast))))
        (unless hl
          (error "No headlines found in %s" ip-company-file))
        (let ((props (append (ip--parse-properties hl))))
          (unless (plist-get props :NAME)
            (error "Company name not found in %s" ip-company-file))
          props))
    (error (message "Error loading company info: %s" err) '())))

;;;###autoload
(defun ip-show-overview ()
  "Show summary of company and client list. Print all properties for debugging."
  (interactive)
  (let ((company (ip-get-company-info))
        (clients (ip-get-clients)))
    (with-current-buffer (get-buffer-create "*IP Overview*")
      (erase-buffer)
      (insert "Company Properties (Debug):\n")
      (if company
          (cl-loop for (key value) on company by #'cddr
                   do (insert (format "%-20s: %s\n" key value)))
        (insert "No company data available\n"))
      (insert "\nSummary:\n")
      (insert (format "Company: %s\n" (or (plist-get company :NAME) "N/A")))
      (insert (format "Address: %s\n" (or (plist-get company :ADDRESS) "N/A")))
      (insert (format "IBAN: %s\n\n" (or (plist-get company :IBAN) "N/A")))
      (insert "Clients:\n")
      (if clients
          (dolist (c clients)
            (insert (format "- %s (%s/hr)\n"
                            (or (plist-get c :NAME) "Unknown")
                            (or (plist-get c :RATE) "?"))))
        (insert "No clients available\n"))
      (goto-char (point-min))
      (read-only-mode 1)
      (pop-to-buffer (current-buffer)))))

(provide 'ip-core)
;;; ip-core.el ends here