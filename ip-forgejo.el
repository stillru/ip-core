;;; ip-forgejo.el --- Forgejo issues integration (Async) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 IP Management System
;; Version: 3.0
;; Keywords: org, forgejo, issues, async

;;; Commentary:
;; Refactored with:
;; - Asynchronous HTTP requests (non-blocking)
;; - Progress reporting
;; - Better error handling
;; - Modular structure

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'org)
(require 'json)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defgroup ip-forgejo nil
  "Forgejo issues integration."
  :group 'org
  :prefix "ip-forgejo-")

(defcustom ip-forgejo-instances
  '(("client" . (("base-url" . "http://git.maketv.internal/api/v1")
                 ("token"    . "b39a2627f82ddea7cce01b7706d8e9e54bb72234"))))
  "Forgejo instances configuration."
  :type '(alist :key-type string :value-type alist)
  :group 'ip-forgejo)

(defcustom ip-forgejo-current-instance "client"
  "Current active Forgejo instance."
  :type 'string
  :group 'ip-forgejo)

(defcustom ip-forgejo-batch-size 5
  "Number of concurrent issue requests."
  :type 'integer
  :group 'ip-forgejo)

;;; ============================================================================
;;; Internal State
;;; ============================================================================

(defvar ip-forgejo--import-state nil
  "State of current import operation.")

(cl-defstruct ip-forgejo-import-state
  buffer           ; Target Org buffer
  issues           ; List of issues to process
  processed        ; Number processed
  total            ; Total count
  inserted         ; Count of new entries
  updated          ; Count of updates
  errors           ; List of errors
  start-time       ; Import start time
  callback)        ; Completion callback

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun ip-forgejo--config ()
  "Get (base-url . token) for current instance."
  (let* ((instance (alist-get ip-forgejo-current-instance 
                              ip-forgejo-instances nil nil #'equal)))
    (unless instance
      (error "Unknown Forgejo instance: %s" ip-forgejo-current-instance))
    (cons (alist-get "base-url" instance nil nil #'equal)
          (alist-get "token" instance nil nil #'equal))))

(defun ip-forgejo--log (level fmt &rest args)
  "Log message to *Forgejo Log* buffer."
  (let ((msg (apply #'format fmt args))
        (timestamp (format-time-string "[%H:%M:%S]"))
        (icon (pcase level
                ('info "ℹ")
                ('success "✓")
                ('warning "⚠")
                ('error "✗"))))
    (with-current-buffer (get-buffer-create "*Forgejo Log*")
      (goto-char (point-max))
      (insert (format "%s %s %s\n" timestamp icon msg)))
    (when (eq level 'error)
      (message "Forgejo error: %s" msg))))

(defun ip-forgejo--progress (state)
  "Update progress message for STATE."
  (let ((pct (/ (* 100 (ip-forgejo-import-state-processed state))
                (max 1 (ip-forgejo-import-state-total state)))))
    (message "Forgejo import: %d%% (%d/%d issues)"
             pct
             (ip-forgejo-import-state-processed state)
             (ip-forgejo-import-state-total state))))

;;; ============================================================================
;;; API Layer - Asynchronous
;;; ============================================================================

(defun ip-forgejo--api-async (url callback &optional error-callback)
  "Send async GET request to URL, call CALLBACK with parsed JSON result.
On error, call ERROR-CALLBACK with error message."
  (let* ((config (ip-forgejo--config))
         (token (cdr config))
         (headers `(("Authorization" . ,(concat "token " token)))))
    
    (request url
      :type "GET"
      :headers headers
      :parser (lambda ()
                (json-parse-string (buffer-string)
                                   :object-type 'alist
                                   :array-type 'list))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (if error-callback
                    (funcall error-callback error-thrown)
                  (ip-forgejo--log 'error "Request failed: %s" error-thrown))))
      :timeout 30)))

;;; ============================================================================
;;; Org Entry Formatting
;;; ============================================================================

(defun ip-forgejo--format-timestamp (iso-str)
  "Convert ISO8601 to Org timestamp."
  (when (and iso-str (not (string-empty-p iso-str)))
    (condition-case nil
        (format-time-string "%Y-%m-%d %a" (date-to-time iso-str))
      (error nil))))

(defun ip-forgejo--format-logbook (times)
  "Format time log entries."
  (when times
    (let ((entries
           (cl-remove nil
                      (mapcar
                       (lambda (entry)
                         (let* ((created (alist-get 'created entry))
                                (duration (or (alist-get 'time entry) 0))
                                (ts (condition-case nil
                                        (date-to-time created)
                                      (error nil))))
                           (when (and ts (> duration 0))
                             (let* ((end-time (time-add ts (seconds-to-time duration)))
                                    (start-str (format-time-string "[%Y-%m-%d %a %H:%M]" ts))
                                    (end-str (format-time-string "[%Y-%m-%d %a %H:%M]" end-time))
                                    (h (/ duration 3600))
                                    (m (/ (% duration 3600) 60)))
                               (format "CLOCK: %s--%s => %02d:%02d"
                                       start-str end-str h m)))))
                       times))))
      (when entries
        (concat ":LOGBOOK:\n"
                (string-join entries "\n")
                "\n:END:\n")))))

(defun ip-forgejo--format-entry (issue times)
  "Format ISSUE with TIMES into Org heading."
  (let* ((title (or (alist-get 'title issue) "Untitled"))
         (number (or (alist-get 'number issue) 0))
         (state (or (alist-get 'state issue) "open"))
         (todo (if (string= state "closed") "DONE" "TODO"))
         (body (or (alist-get 'body issue) ""))
         (repo (alist-get 'repository issue))
         (owner (if (alist-get 'owner repo)
                    (or (alist-get 'login (alist-get 'owner repo)) "unknown")
                  "unknown"))
         (repo-name (or (alist-get 'name repo) "unknown"))
         (config (ip-forgejo--config))
         (base-url (car config))
         (web-url (replace-regexp-in-string "/api/v1$" "" base-url))
         (issue-url (format "%s/%s/%s/issues/%d" web-url owner repo-name number))
         (org-id (org-id-new))
         (total-time (cl-reduce #'+ (mapcar (lambda (e) (or (alist-get 'time e) 0))
                                            times)
                                :initial-value 0))
         (logbook (ip-forgejo--format-logbook times))
         (deadline (alist-get 'deadline issue))
         (deadline-str (when deadline
                        (let ((ts (ip-forgejo--format-timestamp deadline)))
                          (when ts (format "DEADLINE: <%s>" ts))))))
    
    (format "* %s %s    :%s:%s:\n%s\n:PROPERTIES:\n:ID: %s\n:FORGEJO_URL: %s\n:STATE: %s\n:TIME: %d\n:END:\n%s\n%s"
            todo title owner repo-name
            (or deadline-str "")
            org-id issue-url state total-time
            (or logbook "")
            body)))

;;; ============================================================================
;;; Org Buffer Operations
;;; ============================================================================

(defun ip-forgejo--find-entry-by-url (url)
  "Find Org heading with FORGEJO_URL property matching URL."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward "^:FORGEJO_URL: \\(.*\\)$" nil t)
        (when (string= (match-string 1) url)
          (org-back-to-heading t)
          (throw 'found (point))))
      nil)))

(defun ip-forgejo--insert-or-update (url entry)
  "Insert or update entry with URL in current buffer."
  (let ((pos (ip-forgejo--find-entry-by-url url)))
    (if pos
        ;; Update existing
        (progn
          (goto-char pos)
          (let ((beg (point))
                (end (save-excursion
                       (org-end-of-subtree t t)
                       (point))))
            (delete-region beg end)
            (insert entry "\n")
            'updated))
      ;; Insert new
      (progn
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert entry "\n")
        'inserted))))

;;; ============================================================================
;;; Import Logic - Async Orchestration
;;; ============================================================================

(defun ip-forgejo--process-issue (issue state)
  "Process single ISSUE within import STATE."
  (let* ((repo (alist-get 'repository issue))
         (owner (if (alist-get 'owner repo)
                    (or (alist-get 'login (alist-get 'owner repo)) "unknown")
                  "unknown"))
         (repo-name (or (alist-get 'name repo) "unknown"))
         (number (alist-get 'number issue))
         (config (ip-forgejo--config))
         (base-url (car config))
         (times-url (format "%s/repos/%s/%s/issues/%d/times"
                            base-url owner repo-name number)))
    
    ;; Fetch time logs asynchronously
    (ip-forgejo--api-async
     times-url
     (lambda (times)
       ;; Success: format and insert
       (let ((entry (ip-forgejo--format-entry issue times))
             (web-url (replace-regexp-in-string "/api/v1$" "" base-url))
             (issue-url (format "%s/%s/%s/issues/%d" web-url owner repo-name number)))
         
         (with-current-buffer (ip-forgejo-import-state-buffer state)
           (let ((result (ip-forgejo--insert-or-update issue-url entry)))
             (cl-incf (ip-forgejo-import-state-processed state))
             (when (eq result 'inserted)
               (cl-incf (ip-forgejo-import-state-inserted state)))
             (when (eq result 'updated)
               (cl-incf (ip-forgejo-import-state-updated state)))
             
             (ip-forgejo--progress state)
             
             ;; Check if done
             (when (= (ip-forgejo-import-state-processed state)
                      (ip-forgejo-import-state-total state))
               (ip-forgejo--import-complete state))))))
     
     ;; Error callback
     (lambda (err)
       (cl-incf (ip-forgejo-import-state-processed state))
       (push (format "Issue #%d: %s" number err)
             (ip-forgejo-import-state-errors state))
       (ip-forgejo--log 'error "Failed to load times for issue #%d: %s" number err)
       (ip-forgejo--progress state)
       
       ;; Check if done despite error
       (when (= (ip-forgejo-import-state-processed state)
                (ip-forgejo-import-state-total state))
         (ip-forgejo--import-complete state))))))

(defun ip-forgejo--import-complete (state)
  "Handle completion of import STATE."
  (let ((duration (time-subtract (current-time)
                                 (ip-forgejo-import-state-start-time state)))
        (buffer (ip-forgejo-import-state-buffer state)))
    
    (with-current-buffer buffer
      (save-buffer))
    
    (ip-forgejo--log 'success
                     "Import complete: %d inserted, %d updated, %d errors (%.1fs)"
                     (ip-forgejo-import-state-inserted state)
                     (ip-forgejo-import-state-updated state)
                     (length (ip-forgejo-import-state-errors state))
                     (float-time duration))
    
    (message "Forgejo import complete: %d new, %d updated"
             (ip-forgejo-import-state-inserted state)
             (ip-forgejo-import-state-updated state))
    
    ;; Show log buffer if errors
    (when (ip-forgejo-import-state-errors state)
      (display-buffer "*Forgejo Log*"))
    
    ;; Reset state
    (setq ip-forgejo--import-state nil)))

;;; ============================================================================
;;; Public API
;;; ============================================================================

;;;###autoload
(defun ip-forgejo-import-my-issues ()
  "Import assigned issues asynchronously (non-blocking)."
  (interactive)
  
  (when ip-forgejo--import-state
    (user-error "Import already in progress!"))
  
  (let* ((config (ip-forgejo--config))
         (base-url (car config))
         (user-url (format "%s/user" base-url)))
    
    (ip-forgejo--log 'info "Starting import from %s" ip-forgejo-current-instance)
    
    ;; Step 1: Get user info
    (ip-forgejo--api-async
     user-url
     (lambda (user)
       (let ((username (alist-get 'login user))
             (open-url (format "%s/repos/issues/search?assigned=true&state=open" base-url))
             (closed-url (format "%s/repos/issues/search?assigned=true&state=closed" base-url))
             (all-issues nil))
         
         (ip-forgejo--log 'info "Fetching issues for user: %s" username)
         
         ;; Step 2: Fetch open issues
         (ip-forgejo--api-async
          open-url
          (lambda (open-issues)
            (setq all-issues (append all-issues open-issues))
            (ip-forgejo--log 'info "Found %d open issues" (length open-issues))
            
            ;; Step 3: Fetch closed issues
            (ip-forgejo--api-async
             closed-url
             (lambda (closed-issues)
               (setq all-issues (append all-issues closed-issues))
               (ip-forgejo--log 'info "Found %d closed issues" (length closed-issues))
               (ip-forgejo--log 'info "Total: %d issues" (length all-issues))
               
               ;; Step 4: Initialize import state
               (setq ip-forgejo--import-state
                     (make-ip-forgejo-import-state
                      :buffer (current-buffer)
                      :issues all-issues
                      :processed 0
                      :total (length all-issues)
                      :inserted 0
                      :updated 0
                      :errors nil
                      :start-time (current-time)))
               
               ;; Step 5: Process all issues (async)
               (dolist (issue all-issues)
                 (ip-forgejo--process-issue issue ip-forgejo--import-state))
               
               (message "Processing %d issues..." (length all-issues)))
             
             (lambda (err)
               (ip-forgejo--log 'error "Failed to fetch closed issues: %s" err))))
          
          (lambda (err)
            (ip-forgejo--log 'error "Failed to fetch open issues: %s" err)))))
     
     (lambda (err)
       (ip-forgejo--log 'error "Failed to get user info: %s" err)))))

;;;###autoload
(defun ip-forgejo-abort-import ()
  "Abort current import operation."
  (interactive)
  (when ip-forgejo--import-state
    (setq ip-forgejo--import-state nil)
    (message "Forgejo import aborted")))

;;;###autoload
(defun ip-forgejo-switch-instance (instance)
  "Switch to different Forgejo INSTANCE."
  (interactive
   (list (completing-read "Instance: "
                          (mapcar #'car ip-forgejo-instances)
                          nil t)))
  (setq ip-forgejo-current-instance instance)
  (message "Switched to Forgejo instance: %s" instance))

;;; ============================================================================
;;; Minor Mode
;;; ============================================================================

(defvar ip-forgejo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f i") 'ip-forgejo-import-my-issues)
    (define-key map (kbd "C-c f s") 'ip-forgejo-switch-instance)
    (define-key map (kbd "C-c f a") 'ip-forgejo-abort-import)
    map)
  "Keymap for `ip-forgejo-mode'.")

;;;###autoload
(define-minor-mode ip-forgejo-mode
  "Minor mode for Forgejo integration."
  :lighter " Forgejo"
  :keymap ip-forgejo-mode-map
  :group 'ip-forgejo)

(provide 'ip-forgejo)

;;; ip-forgejo.el ends here