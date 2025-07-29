;;; ip-debug.el --- Unified debug logging for IP system -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified logging and debugging utilities for the IP task/invoice system.
;; This is an optional module that provides enhanced logging capabilities.
;; If not loaded, modules fall back to minimal logging via `message'.

;;; Code:

(require 'org-element)
(require 'ip-core)
(require 'cl-lib)

(defgroup ip-debug nil
  "Unified logging and debugging utilities for the IP task/invoice system."
  :group 'ip-core)

(defcustom ip-debug-enabled t
  "Whether to enable debug logging."
  :type 'boolean
  :group 'ip-debug)

(defcustom ip-debug-main-buffer "*IP Debug Log*"
  "Main buffer name used for unified logging messages."
  :type 'string
  :group 'ip-debug)

(defcustom ip-debug-module-buffers t
  "Whether to create separate buffers for each module."
  :type 'boolean
  :group 'ip-debug)

(defvar ip-debug--module-buffers '()
  "Alist of (MODULE . BUFFER-NAME) for module-specific buffers.")

(defvar ip-tasks-file nil
  "Path to the Org file containing IP tasks. Provided by `ip-core.el`.")

(declare-function ip--load-org-file "ip-core.el")
(declare-function ip--task-in-range-p "ip-core.el")
(declare-function ip--parse-task-detailed "ip-core.el")


(defun ip-debug--get-module-buffer (module)
  "Get or create buffer name for MODULE."
  (when ip-debug-module-buffers
    (let ((buffer-name (alist-get module ip-debug--module-buffers)))
      (unless buffer-name
        (setq buffer-name (format "*IP Debug %s*" (capitalize (symbol-name module))))
        (push (cons module buffer-name) ip-debug--module-buffers))
      buffer-name)))

(defun ip-debug-log (level module message &rest args)
  "Log MESSAGE with LEVEL from MODULE to debug buffers.
LEVEL can be \='info, \='success, \='warning, or \='error.
MODULE should be a symbol like \='core, \='forgejo, \='invoice."
  (when ip-debug-enabled
    (let* ((formatted-msg (apply #'format message args))
           (timestamp (format-time-string "[%H:%M:%S] "))
           (module-str (format "[%s] " (upcase (symbol-name module))))
           (icon (pcase level
                   ('info "ℹ️")
                   ('success "✅")
                   ('warning "⚠️")
                   ('error "❌")
                   (_ "🔍")))
           (full-message (format "%s%s %s%s\n" timestamp icon module-str formatted-msg)))
      
      ;; Log to main unified buffer
      (with-current-buffer (get-buffer-create ip-debug-main-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert full-message)))
      
      ;; Log to module-specific buffer if enabled
      (when-let ((module-buffer (ip-debug--get-module-buffer module)))
        (with-current-buffer (get-buffer-create module-buffer)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "%s%s %s\n" timestamp icon formatted-msg))))))))

(defmacro ip-debug (module message &rest args)
  "Log MESSAGE from MODULE as \='info\=' level using \=`ip-debug-log\='."
  `(ip-debug-log 'info ,module ,message ,@args))

(defun ip-debug-show (&optional module)
  "Display the debug buffer.
If MODULE is specified, show that module\'s buffer, otherwise show main buffer."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Module: " 
                                    (mapcar (lambda (x) (symbol-name (car x))) 
                                            ip-debug--module-buffers))))))
  (let ((buffer-name (if module
                         (ip-debug--get-module-buffer module)
                       ip-debug-main-buffer)))
    (pop-to-buffer buffer-name)))

(defun ip-debug-clear (&optional module)
  "Clear the debug buffer.
If MODULE is specified, clear that module's buffer, otherwise clear main buffer."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Module: " 
                                    (mapcar (lambda (x) (symbol-name (car x))) 
                                            ip-debug--module-buffers))))))
  (let ((buffer-name (if module
                         (ip-debug--get-module-buffer module)
                       ip-debug-main-buffer)))
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun ip-debug-clear-all ()
  "Clear all debug buffers."
  (interactive)
  (ip-debug-clear)  ; Clear main buffer
  (dolist (entry ip-debug--module-buffers)
    (when (get-buffer (cdr entry))
      (with-current-buffer (cdr entry)
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun ip-debug-list-buffers ()
  "Show list of all debug buffers."
  (interactive)
  (let ((buffers (cons ip-debug-main-buffer 
                       (mapcar #'cdr ip-debug--module-buffers))))
    (message "IP Debug buffers: %s" (string-join buffers ", "))))

;; Provide fallback functions for when ip-debug is not loaded
(defvar ip-debug-fallback-functions
  '((ip-debug-log . ip-debug--fallback-log)
    (ip-debug . ip-debug--fallback-debug))
  "Fallback functions when ip-debug is not available.")

(defun ip-debug--fallback-log (level module message &rest args)
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

(defmacro ip-debug--fallback-debug (module message &rest args)
  "Fallback debug macro that uses `message'."
  `(ip-debug--fallback-log 'info ,module ,message ,@args))

;;; Invoice-specific debug functions

(defun ip-debug-client-properties ()
  "Debug function to examine all CLIENT properties in tasks."
  (interactive)
  (let ((ast (ip--load-org-file ip-tasks-file))
        (clients-found (make-hash-table :test 'equal))
        (total-tasks 0)
        (valid-client-ids (ip-list-client-ids)))  ; Get valid client IDs
    (ip-debug-log 'info 'debug "=== Debugging Client Properties ===")
    (org-element-map ast 'headline
      (lambda (hl)
        (cl-incf total-tasks)
        (let* ((props (org-element-property :properties hl))
               (client-raw (cdr (assoc-string "CLIENT" props t)))
               ;; Check tags if CLIENT property is not found
               (tags (org-element-property :tags hl))
               (client-from-tags (cl-find-if
                                  (lambda (tag)
                                    (member (ip--normalize-tag tag) valid-client-ids))
                                  tags))
               (client (or client-raw
                           (when client-from-tags
                             (ip--normalize-tag client-from-tags))
                           "unknown"))
               (title (org-element-property :raw-value hl)))
          (puthash client (1+ (gethash client clients-found 0)) clients-found)
          (ip-debug-log 'info 'debug "--- Task %d: %s ---" total-tasks title)
          (ip-debug-log 'info 'debug "  Raw CLIENT property: %S" client-raw)
          (ip-debug-log 'info 'debug "  Tags: %S" tags)
          (ip-debug-log 'info 'debug "  Processed client: %S" client)
          (ip-debug-log 'info 'debug "  All properties: %S" props)
          (ip-debug-log 'info 'debug "  Has clock entries: %s"
                        (if (org-element-map hl 'clock #'identity) "YES" "NO")))))
    (ip-debug-log 'info 'debug "=== All Clients Found (Summary) ===")
    (maphash (lambda (client count)
               (ip-debug-log 'info 'debug "  %s: %d tasks" client count))
             clients-found)
    (ip-debug-log 'info 'debug "Total tasks processed: %d" total-tasks)))

(defun ip-debug-task-detailed-parsing ()
  "Debug the detailed task parsing specifically."
  (interactive)
  (let* ((start "2025-07-01")
         (end "2025-07-30")
         (ast (ip--load-org-file ip-tasks-file))
         (all-tasks 0)
         (tasks-in-range 0)
         (maketv-tasks 0))
    (ip-debug-log 'info 'debug "=== Debugging Detailed Task Parsing ===")
    (org-element-map ast 'headline
      (lambda (hl)
        (cl-incf all-tasks)
        (when (ip--task-in-range-p hl start end)
          (cl-incf tasks-in-range)
          (let* ((parsed (ip--parse-task-detailed hl))
                 (client (plist-get parsed :client))
                 (title (plist-get parsed :title)))
            (ip-debug-log 'info 'debug "Task %d: %s" tasks-in-range title)
            (ip-debug-log 'info 'debug "  Client: %S" client)
            (when (string= client "maketv")
              (cl-incf maketv-tasks)
              (ip-debug-log 'success 'debug "  *** THIS IS A MAKETV TASK ***"))))))
    (ip-debug-log 'info 'debug "=== Summary ===")
    (ip-debug-log 'info 'debug "All tasks: %d" all-tasks)
    (ip-debug-log 'info 'debug "Tasks in range: %d" tasks-in-range)
    (ip-debug-log 'info 'debug "MakeTV tasks in range: %d" maketv-tasks)))

(defun ip-debug-string-comparison ()
  "Debug string comparison for client matching."
  (interactive)
  (let ((test-strings '("maketv" "MAKETV" "MakeTV" " maketv " "maketv\n")))
    (ip-debug-log 'info 'debug "=== String Comparison Debug ===")
    (dolist (test test-strings)
      (ip-debug-log 'info 'debug "Testing: %S" test)
      (ip-debug-log 'info 'debug "  string= with 'maketv': %s" (string= test "maketv"))
      (ip-debug-log 'info 'debug "  string-equal with 'maketv': %s" (string-equal test "maketv"))
      (ip-debug-log 'info 'debug "  after string-trim: %S" (string-trim test))
      (ip-debug-log 'info 'debug "  after downcase: %S" (downcase test)))))

;; Enhanced version of the parsing function with better debugging
(defun ip--parse-task-detailed-debug (task)
  "Extract detailed info from TASK headline with debugging."
  (condition-case err
      (let* ((props (org-element-property :properties task))
             (client-raw (cdr (assoc-string "CLIENT" props t)))
             (client (or client-raw "unknown"))
             (service (or (cdr (assoc-string "REPO" props t)) "general"))
             (title (org-element-property :raw-value task)))
        (ip-debug-log 'info 'debug "  Parsing task: %s" title)
        (ip-debug-log 'info 'debug "    Raw CLIENT: %S" client-raw)
        (ip-debug-log 'info 'debug "    Processed CLIENT: %S" client)
        (ip-debug-log 'info 'debug "    String= maketv: %s" (string= client "maketv"))
        (list :title title :client client :service service :element task))
    (error 
     (ip-debug-log 'error 'debug "Error parsing detailed task: %s" (error-message-string err))
     nil)))

;; Test function to run the detailed parsing with debug output
(defun ip-debug-test-detailed-parsing ()
  "Test detailed parsing with debug output."
  (interactive)
  (let* ((start "2025-07-01")
         (end "2025-07-30")
         (ast (ip--load-org-file ip-tasks-file))
         (count 0))
    (ip-debug-log 'info 'debug "=== Testing Detailed Parsing with Debug ===")
    (cl-loop for hl in (org-element-map ast 'headline #'identity)
             when (ip--task-in-range-p hl start end)
             do (progn
                  (cl-incf count)
                  (ip-debug-log 'info 'debug "=== Task %d ===" count)
                  (let ((parsed (ip--parse-task-detailed-debug hl)))
                    (when (and parsed (string= (plist-get parsed :client) "maketv"))
                      (ip-debug-log 'success 'debug "*** FOUND MAKETV TASK! ***")))))))

(provide 'ip-debug)
;;; ip-debug.el ends here