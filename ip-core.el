;;; ip-core.el --- Core module for IP management in Org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Core utilities for IP Manager, including client and file handling.
;; This module provides the foundation for the IP management system.

;;; Code:
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)

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

;; Cache variables
(defvar ip--company-cache nil
  "Cache for company data.")

(defvar ip--company-cache-mtime nil
  "Modification time when company cache was last updated.")

(defvar ip--clients-cache nil
  "Cache for clients data.")

(defvar ip--clients-cache-mtime nil
  "Modification time when clients cache was last updated.")

;; Utility functions
(defun ip--plist-remove-duplicates (plist)
  "Remove duplicate keys from PLIST, keeping the LAST occurrence."
  (let ((hash (make-hash-table :test 'equal)))
    ;; Проходим по всем парам и записываем в хеш — последнее значение перезапишет предыдущие
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (puthash key value hash))
      (setq plist (cddr plist)))
    ;; Преобразуем хеш обратно в plist
    (let (result)
      (maphash (lambda (key value)
                 (push value result)
                 (push key result))
               hash)
      (nreverse result))))

(defun ip--get-full-path (filename)
  "Get full path for FILENAME in `ip-org-directory'."
  (expand-file-name filename ip-org-directory))

(defun ip--file-newer-than-cache-p (file cache-mtime)
  "Check if FILE is newer than CACHE-MTIME."
  (let ((file-mtime (when (file-exists-p file)
                      (nth 5 (file-attributes file)))))
    (or (null cache-mtime)
        (null file-mtime)
        (time-less-p cache-mtime file-mtime))))

(defun ip--ensure-directory ()
  "Ensure IP org directory exists."
  (unless (file-directory-p ip-org-directory)
    (make-directory ip-org-directory t)
    (ip-debug-log 'info 'core "Created IP directory: %s" ip-org-directory)))

(defun ip--load-org-file (filename)
  "Return parsed Org AST from FILENAME in `ip-org-directory'."
  (let ((path (if (file-name-absolute-p filename)
                  filename
                (ip--get-full-path filename))))
    (ip-debug-log 'info 'core "Checking file existence: %s" path)
    (unless (file-exists-p path)
      (ip-debug-log 'error 'core "File does not exist: %s" path)
      (error "File %s does not exist" path))
    (ip-debug-log 'info 'core "Loading org file: %s" path)
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8)
      (insert-file-contents path)
      (org-mode) ; Ensure Org-mode parsing context
      (org-element-parse-buffer))))

(defun ip--get-headlines (ast &optional level)
  "Extract headlines from AST at specified LEVEL (default 1)."
  (let ((target-level (or level 1)))
    (org-element-map ast 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) target-level)
          hl)))))

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
  (when name
    (downcase
     (replace-regexp-in-string "[^a-zA-Z0-9]" "" name))))

(defun ip--parse-services (client-hl)
  "Parse services from CLIENT-HL sub-headlines."
  (org-element-map client-hl 'headline
    (lambda (sub-hl)
      (when (= (org-element-property :level sub-hl) 2)
        (let* ((service-name (org-element-property :raw-value sub-hl))
               (props (ip--parse-properties sub-hl))
               (tag (or (plist-get props :TAG)
                        (ip--normalize-tag service-name))))
          (ip-debug-log 'info 'core "Parsed service: %s, tag: %s" service-name tag)
          (append
           (list :description service-name
                 :tag tag)
           props))))))

(defun ip--load-clients-data ()
  "Load client data from clients file with caching."
  (let ((clients-path (ip--get-full-path ip-clients-file)))
    (when (ip--file-newer-than-cache-p clients-path ip--clients-cache-mtime)
      (ip-debug-log 'info 'core "Reloading clients data from: %s" clients-path)
      (condition-case err
          (progn
            (setq ip--clients-cache-mtime (when (file-exists-p clients-path)
                                            (nth 5 (file-attributes clients-path))))
            (if (file-exists-p clients-path)
                (let ((ast (ip--load-org-file clients-path)))
                  (setq ip--clients-cache
                        (cl-loop for hl in (ip--get-headlines ast)
                                 collect (let* ((client-name (org-element-property :raw-value hl))
                                                (props (ip--parse-properties hl))
                                                (client-id (or (plist-get props :ID)
                                                               (ip--normalize-tag client-name)))
                                                (services (ip--parse-services hl)))
                                           (ip-debug-log 'info 'core "Loaded client: %s (ID: %s)" client-name client-id)
                                           (ip--plist-remove-duplicates
                                           (append (list :NAME client-name :ID client-id)
                                                   props
                                                   (when services (list :services services)))))))
                  (ip-debug-log 'success 'core "Loaded %d clients" (length ip--clients-cache)))
              (progn
                (ip-debug-log 'warning 'core "Clients file not found: %s" clients-path)
                (setq ip--clients-cache nil))))
        (error
         (ip-debug-log 'error 'core "Error loading clients data: %s" (error-message-string err))
         (setq ip--clients-cache nil))))
    ip--clients-cache))

(defun ip--load-company-data ()
  "Load company data from company file with caching."
  (let ((company-path (ip--get-full-path ip-company-file)))
    (when (ip--file-newer-than-cache-p company-path ip--company-cache-mtime)
      (ip-debug-log 'info 'core "Reloading company data from: %s" company-path)
      (condition-case err
          (progn
            (setq ip--company-cache-mtime (when (file-exists-p company-path)
                                            (nth 5 (file-attributes company-path))))
            (if (file-exists-p company-path)
                (let* ((ast (ip--load-org-file company-path))
                       (hl (car (ip--get-headlines ast))))
                  (if hl
                      (let ((props (ip--parse-properties hl)))
                        (setq ip--company-cache
                              (ip--plist-remove-duplicates
                              (append (list :NAME (org-element-property :raw-value hl))
                                      props)))
                        (ip-debug-log 'success 'core "Loaded company data: %s" 
                                      (plist-get ip--company-cache :NAME)))
                    (progn
                      (ip-debug-log 'warning 'core "No headlines found in %s" company-path)
                      (setq ip--company-cache nil))))
              (progn
                (ip-debug-log 'warning 'core "Company file not found: %s" company-path)
                (setq ip--company-cache nil))))
        (error
         (ip-debug-log 'error 'core "Error loading company data: %s" (error-message-string err))
         (setq ip--company-cache nil))))
    ip--company-cache))

(defun ip-get-clients (&optional refresh)
  "Return list of clients as plists from `clients.org'.
If REFRESH is non-nil, force reload from file."
  (ip--ensure-directory)
  (when refresh
    (setq ip--clients-cache nil
          ip--clients-cache-mtime nil)
    (ip-debug-log 'info 'core "Force refreshing clients cache"))
  (ip--load-clients-data))

(defun ip-get-company-info (&optional refresh)
  "Return company info from `company.org' as plist (first headline).
If REFRESH is non-nil, force reload from file."
  (ip--ensure-directory)
  (when refresh
    (setq ip--company-cache nil
          ip--company-cache-mtime nil)
    (ip-debug-log 'info 'core "Force refreshing company cache"))
  (ip--load-company-data))

(defun ip-get-client-by-id (client-id)
  "Get client data by CLIENT-ID."
  (ip-debug-log 'info 'core "Looking up client: %s" client-id)
  (let ((client (cl-find client-id (ip-get-clients)
           :key (lambda (c) (plist-get c :ID))
                         :test 'equal)))
    (if client
        (ip-debug-log 'info 'core "Found client: %s (ID: %s)" (plist-get client :NAME) client-id)
      (ip-debug-log 'warning 'core "Client not found: %s" client-id))
    client))

(defun ip-get-client-service (client-id service-tag)
  "Get service data for CLIENT-ID and SERVICE-TAG."
  (ip-debug-log 'info 'core "Looking up service %s for client %s" service-tag client-id)
  (let ((client (ip-get-client-by-id client-id)))
    (when client
      (let ((service (cl-find service-tag (plist-get client :services)
               :key (lambda (s) (plist-get s :tag))
                              :test 'equal)))
        (if service
            (ip-debug-log 'info 'core "Found service: %s (tag: %s)" (plist-get service :description) service-tag)
          (ip-debug-log 'warning 'core "Service not found: %s for client %s" service-tag client-id))
        service))))

(defun ip-list-client-ids ()
  "Return list of all client IDs."
  (mapcar (lambda (c) (plist-get c :ID)) (ip-get-clients)))

(defun ip-list-service-tags ()
  "Return list of all service tags across all clients."
  (cl-loop for client in (ip-get-clients)
           when (plist-get client :services)
           append (mapcar (lambda (s) (plist-get s :tag))
                          (plist-get client :services))))

(defun ip-validate-setup ()
  "Validate that the IP system is properly set up."
  (ip-debug-log 'info 'core "Starting IP system validation")
  (let ((issues '()))
    (unless (file-directory-p ip-org-directory)
      (push (format "Directory does not exist: %s" ip-org-directory) issues))
    
    (let ((company-file (ip--get-full-path ip-company-file)))
      (unless (file-exists-p company-file)
        (push (format "Company file missing: %s" company-file) issues)))
    
    (let ((clients-file (ip--get-full-path ip-clients-file)))
      (unless (file-exists-p clients-file)
        (push (format "Clients file missing: %s" clients-file) issues)))
    
    (let ((tasks-file (ip--get-full-path ip-tasks-file)))
      (unless (file-exists-p tasks-file)
        (push (format "Tasks file missing: %s" tasks-file) issues)))
    
    (when issues
      (ip-debug-log 'error 'core "Setup validation failed: %s" issues)
      (error "IP system setup issues:\n%s" (mapconcat 'identity issues "\n")))
    
    ;; Try to load data
    (condition-case err
        (ip-get-company-info)
      (error (push (format "Error loading company data: %s" 
                           (error-message-string err)) issues)))
    
    (condition-case err
        (ip-get-clients)
      (error (push (format "Error loading clients data: %s" 
                           (error-message-string err)) issues)))
    
    (if issues
        (progn
          (ip-debug-log 'error 'core "Data validation failed: %s" issues)
          (error "IP system validation failed:\n%s" (mapconcat 'identity issues "\n")))
      (ip-debug-log 'success 'core "IP system validation successful")
      (message "IP system validation successful"))))

;;;###autoload
(defun ip-show-overview ()
  "Show summary of company and client list. Print all properties for debugging."
  (interactive)
  (ip-debug-log 'info 'core "Generating system overview")
  (condition-case err
      (let ((company (ip-get-company-info))
            (clients (ip-get-clients)))
        (with-current-buffer (get-buffer-create "*IP Overview*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "=== IP Management System Overview ===\n\n")
            
            ;; Company section
            (insert "Company Information:\n")
            (insert (make-string 40 ?-) "\n")
            (if company
                (progn
                  (insert (format "Name: %s\n" (or (plist-get company :NAME) "N/A")))
                  (insert (format "Address: %s\n" (or (plist-get company :ADDRESS) "N/A")))
                  (insert (format "Email: %s\n" (or (plist-get company :EMAIL) "N/A")))
                  (insert (format "Phone: %s\n" (or (plist-get company :PHONE) "N/A")))
                  (insert (format "IBAN: %s\n" (or (plist-get company :IBAN) "N/A")))
                  (insert (format "Tax ID: %s\n" (or (plist-get company :TAX_ID) "N/A")))
                  (insert "\nAll Company Properties (Debug):\n")
                  (cl-loop for (key value) on company by #'cddr
                           do (insert (format "  %-20s: %s\n" key value))))
              (insert "No company data available\n"))
            
            (insert "\n\nClients:\n")
            (insert (make-string 40 ?-) "\n")
            (if clients
                (progn
                  (insert (format "Total clients: %d\n\n" (length clients)))
                  (dolist (c clients)
                    (insert (format "• %s (ID: %s)\n"
                                    (or (plist-get c :NAME) "Unknown")
                                    (or (plist-get c :ID) "N/A")))
                    (when (plist-get c :EMAIL)
                      (insert (format "  Email: %s\n" (plist-get c :EMAIL))))
                    (when (plist-get c :DEFAULT_RATE)
                      (insert (format "  Default Rate: %s %s/hr\n" 
                                      (plist-get c :DEFAULT_RATE)
                                      (or (plist-get c :CURRENCY) "EUR"))))
                    (when (plist-get c :services)
                      (insert "  Services:\n")
                      (dolist (s (plist-get c :services))
                        (insert (format "    - %s (tag: %s, rate: %s)\n"
                                        (plist-get s :description)
                                        (plist-get s :tag)
                                        (or (plist-get s :RATE) "default")))))
                    (insert "\n")))
              (insert "No clients available\n"))
            
            ;; System info
            (insert "\nSystem Information:\n")
            (insert (make-string 40 ?-) "\n")
            (insert (format "Org Directory: %s\n" ip-org-directory))
            (insert (format "Company File: %s\n" (ip--get-full-path ip-company-file)))
            (insert (format "Clients File: %s\n" (ip--get-full-path ip-clients-file)))
            (insert (format "Tasks File: %s\n" (ip--get-full-path ip-tasks-file)))
            
            (goto-char (point-min))
            (read-only-mode 1))
          (display-buffer (current-buffer))
          (ip-debug-log 'success 'core "Overview generated successfully")))
    (error 
     (ip-debug-log 'error 'core "Error generating overview: %s" (error-message-string err))
     (message "Error generating overview: %s" (error-message-string err)))))

;;;###autoload
(defun ip-setup-files ()
  "Create template files for IP system if they don't exist."
  (interactive)
  (ip-debug-log 'info 'core "Setting up IP system files")
  (ip--ensure-directory)
  
  ;; Create company.org template
  (let ((company-file (ip--get-full-path ip-company-file)))
    (unless (file-exists-p company-file)
      (with-temp-file company-file
        (insert "#+TITLE: Company Information\n\n")
        (insert "* My Company\n")
        (insert ":PROPERTIES:\n")
        (insert ":NAME: My Company Name\n")
        (insert ":ADDRESS: 123 Main St, City, Country\n")
        (insert ":EMAIL: contact@mycompany.com\n")
        (insert ":PHONE: +1-555-0123\n")
        (insert ":IBAN: DE89370400440532013000\n")
        (insert ":TAX_ID: 123456789\n")
        (insert ":END:\n\n")
        (insert "Company description and notes go here.\n"))
      (ip-debug-log 'success 'core "Created company template: %s" company-file)))
  
  ;; Create clients.org template
  (let ((clients-file (ip--get-full-path ip-clients-file)))
    (unless (file-exists-p clients-file)
      (with-temp-file clients-file
        (insert "#+TITLE: Clients\n\n")
        (insert "* Example Client\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: client1\n")
        (insert ":EMAIL: client@example.com\n")
        (insert ":ADDRESS: 456 Client St, Client City, Country\n")
        (insert ":CURRENCY: EUR\n")
        (insert ":DEFAULT_RATE: 50\n")
        (insert ":TAX_RATE: 20\n")
        (insert ":PAYMENT_DETAILS: Bank transfer to IBAN: DE89370400440532013000\n")
        (insert ":END:\n\n")
        (insert "** Development\n")
        (insert ":PROPERTIES:\n")
        (insert ":TAG: dev\n")
        (insert ":RATE: 60\n")
        (insert ":TAXABLE: t\n")
        (insert ":END:\n\n")
        (insert "** Consulting\n")
        (insert ":PROPERTIES:\n")
        (insert ":TAG: consulting\n")
        (insert ":RATE: 80\n")
        (insert ":TAXABLE: t\n")
        (insert ":END:\n\n"))
      (ip-debug-log 'success 'core "Created clients template: %s" clients-file)))
  
  ;; Create tasks.org template
  (let ((tasks-file (ip--get-full-path ip-tasks-file)))
    (unless (file-exists-p tasks-file)
      (with-temp-file tasks-file
        (insert "#+TITLE: Tasks\n\n")
        (insert "* Example Task :client1:dev:\n")
        (insert ":PROPERTIES:\n")
        (insert ":CLIENT: client1\n")
        (insert ":REPO: dev\n")
        (insert ":END:\n")
        (insert "CLOCK: [2025-01-01 Mon 09:00]--[2025-01-01 Mon 17:00] =>  8:00\n")
        (insert "\nTask description goes here.\n\n")
        (insert "* Another Task :client1:consulting:\n")
        (insert ":PROPERTIES:\n")
        (insert ":CLIENT: client1\n")
        (insert ":REPO: consulting\n")
        (insert ":END:\n")
        (insert "CLOCK: [2025-01-02 Tue 10:00]--[2025-01-02 Tue 12:00] =>  2:00\n")
        (insert "\nConsulting work description.\n\n"))
      (ip-debug-log 'success 'core "Created tasks template: %s" tasks-file)))
  
  (ip-debug-log 'success 'core "IP system files setup completed")
  (message "IP system files created successfully!"))

;;;###autoload
(defun ip-refresh-cache ()
  "Refresh all caches."
  (interactive)
  (setq ip--company-cache nil
        ip--company-cache-mtime nil
        ip--clients-cache nil
        ip--clients-cache-mtime nil)
  (ip-debug-log 'success 'core "All caches refreshed")
  (message "IP core cache refreshed"))

(provide 'ip-core)
;;; ip-core.el ends here