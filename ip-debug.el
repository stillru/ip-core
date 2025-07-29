;;; ip-debug.el --- Unified debug logging for IP system -*- lexical-binding: t; -*-

(defgroup ip-debug nil
  "Unified logging and debugging utilities for the IP task/invoice system."
  :group 'ip-core)

(defcustom ip-debug-enabled t
  "Whether to enable debug logging."
  :type 'boolean
  :group 'ip-debug)

(defcustom ip-debug-buffer "*IP Debug Log*"
  "Buffer name used for logging messages."
  :type 'string
  :group 'ip-debug)

(defun ip-debug-log (level message &rest args)
  "Log MESSAGE with LEVEL to `ip-debug-buffer'."
  (when ip-debug-enabled
    (let ((formatted-msg (apply #'format message args))
          (timestamp (format-time-string "[%H:%M:%S] "))
          (icon (pcase level
                  ('info    "ℹ️")
                  ('success "✅")
                  ('warning "⚠️")
                  ('error   "❌")
                  (_        "🔍"))))
      (with-current-buffer (get-buffer-create ip-debug-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "%s %s %s\n" timestamp icon formatted-msg)))))))

(defmacro ip-debug (message &rest args)
  "Log MESSAGE as 'info' level using `ip-debug-log'."
  `(ip-debug-log 'info ,message ,@args))

(defun ip-debug-show ()
  "Display the debug buffer."
  (interactive)
  (pop-to-buffer ip-debug-buffer))

(defun ip-debug-clear ()
  "Clear the debug buffer."
  (interactive)
  (with-current-buffer (get-buffer-create ip-debug-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(provide 'ip-debug)
;;; ip-debug.el ends here
