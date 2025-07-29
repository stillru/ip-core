;;; ip-debug.el --- Unified debug logging for IP system -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified logging and debugging utilities for the IP task/invoice system.
;; This is an optional module that provides enhanced logging capabilities.
;; If not loaded, modules fall back to minimal logging via `message'.

;;; Code:

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
LEVEL can be 'info, 'success, 'warning, or 'error.
MODULE should be a symbol like 'core, 'forgejo, 'invoice."
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
  "Log MESSAGE from MODULE as 'info' level using `ip-debug-log'."
  `(ip-debug-log 'info ,module ,message ,@args))

(defun ip-debug-show (&optional module)
  "Display the debug buffer.
If MODULE is specified, show that module's buffer, otherwise show main buffer."
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

(provide 'ip-debug)
;;; ip-debug.el ends here