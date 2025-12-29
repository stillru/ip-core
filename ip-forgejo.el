;;; ip-forgejo.el --- Forgejo issues integration (Async) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 IP Management System
;; Version: 3.2
;; Keywords: org, forgejo, issues, async

;;; Commentary:
;; Refactored with:
;; - Asynchronous HTTP requests (non-blocking)
;; - Progress reporting
;; - Better error handling
;; - Modular structure
;; - Insertion/update using Org API (properties, stable IDs, preserves subtrees)
;; - Single planning line
;; - Body/logbook replacement without destroying manual sub-headings

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

;;; ============================================================================
;;; Internal State
;;; ============================================================================

(defvar ip-forgejo--import-state nil
  "State of current import operation.")

(defvar ip-forgejo--import-timeout-timer nil
  "Timer for import timeout watchdog.")

(cl-defstruct ip-forgejo-import-state
  buffer           ; Target Org buffer
  issues           ; List of issues to process
  processed        ; Number processed
  total            ; Total count
  inserted         ; Count of new entries
  updated          ; Count of updates
  errors           ; List of errors
  start-time       ; Import start time
  last-activity)   ; Last activity timestamp

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
    (when (memq level '(error warning))
      (message "Forgejo %s: %s" level msg))))

(defun ip-forgejo--progress (state)
  "Update progress message for STATE."
  (when state
    ;; Update last activity timestamp
    (setf (ip-forgejo-import-state-last-activity state) (current-time))
    
    (let* ((processed (ip-forgejo-import-state-processed state))
           (total (ip-forgejo-import-state-total state))
           (pct (if (> total 0)
                    (/ (* 100 processed) total)
                  0)))
      (message "Forgejo import: %d%% (%d/%d issues, %d new, %d updated)"
               pct processed total
               (ip-forgejo-import-state-inserted state)
               (ip-forgejo-import-state-updated state)))))

(defun ip-forgejo--check-stalled ()
  "Check if import has stalled and force completion if needed."
  (when ip-forgejo--import-state
    (let* ((now (current-time))
           (last (ip-forgejo-import-state-last-activity ip-forgejo--import-state))
           (idle-seconds (time-to-seconds (time-subtract now last))))
      
      ;; If no activity for 10 seconds, consider it stalled
      (when (> idle-seconds 10)
        (ip-forgejo--log 'warning 
                         "Import stalled for %d seconds. Forcing completion..."
                         idle-seconds)
        (ip-forgejo--import-complete ip-forgejo--import-state t)))))

;;; ============================================================================
;;; API Layer - Asynchronous
;;; ============================================================================

(defun ip-forgejo--api-async (url callback &optional error-callback)
  "Send async GET request to URL, call CALLBACK with parsed JSON result.
On error, call ERROR-CALLBACK with error message."
  (let* ((config (ip-forgejo--config))
         (token (cdr config))
         (headers `(("Authorization" . ,(concat "token " token))))
         (request-called nil)
         (request-start (current-time)))
    
    (ip-forgejo--log 'info "→ REQUEST: %s" url)
    
    (request url
      :type "GET"
      :headers headers
      :parser (lambda ()
                (condition-case err
                    (let ((json-str (buffer-string)))
                      (if (string-empty-p (string-trim json-str))
                          (progn
                            (ip-forgejo--log 'info "← PARSE: empty response, returning empty list")
                            '())  
                        (progn
                          (ip-forgejo--log 'info "← PARSE: %d bytes" (length json-str))
                          (json-parse-string json-str
                                             :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object :false))))
                  (error
                   (ip-forgejo--log 'error "← PARSE ERROR: %s" err)
                   '())))
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  (setq request-called t)
                  (let* ((duration (float-time (time-subtract (current-time) request-start)))
                         (status (request-response-status-code response))
                         (data-type (type-of data))
                         (data-length (cond
                                       ((listp data) (length data))
                                       ((vectorp data) (length data))
                                       ((stringp data) (length data))
                                       ((null data) 0)
                                       (t 0))))
                    (ip-forgejo--log 'success "← SUCCESS: status=%s, type=%s, length=%d, time=%.2fs"
                                     status data-type data-length duration)
                    (funcall callback (or data '())))))
      :error (cl-function
              (lambda (&key error-thrown response symbol-status &allow-other-keys)
                (setq request-called t)
                (let* ((duration (float-time (time-subtract (current-time) request-start)))
                       (status (if response
                                   (request-response-status-code response)
                                 "no-response"))
                       (err-msg (format "%s" error-thrown)))
                  (ip-forgejo--log 'error "← ERROR: status=%s, symbol=%s, error=%s, time=%.2fs"
                                   status symbol-status err-msg duration)
                  (if error-callback
                      (funcall error-callback err-msg)
                    (ip-forgejo--log 'error "No error callback for: %s" url)))))
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (let ((duration (float-time (time-subtract (current-time) request-start)))
                         (status (if response
                                     (request-response-status-code response)
                                   "no-response")))
                     (ip-forgejo--log 'info "← COMPLETE: status=%s, called=%s, time=%.2fs"
                                      status request-called duration)
                     (unless request-called
                       (ip-forgejo--log 'warning "← NO CALLBACK INVOKED for: %s" url)
                       (when error-callback
                         (funcall error-callback "Request completed without callback"))))))
      :timeout 15)
    
    (run-with-timer 20 nil
                    (lambda ()
                      (unless request-called
                        (let ((duration (float-time (time-subtract (current-time) request-start))))
                          (ip-forgejo--log 'error "← TIMEOUT (20s): %s, time=%.2fs" url duration)
                          (when error-callback
                            (funcall error-callback "Request timeout"))))))))

;;; ============================================================================
;;; Data Extraction Helpers
;;; ============================================================================

(defun ip-forgejo--safe-get (alist key &optional default)
  "Safely get KEY from ALIST, return DEFAULT if not found."
  (or (alist-get key alist) default))

(defun ip-forgejo--extract-owner (repo)
  "Extract owner name from REPO alist, handling different formats."
  (let ((owner (alist-get 'owner repo)))
    (cond
     ((stringp owner) owner)
     ((and owner (listp owner))
      (or (alist-get 'login owner)
          (alist-get 'username owner)
          "unknown"))
     (t "unknown"))))

(defun ip-forgejo--extract-repo-name (repo)
  "Extract repository name from REPO alist."
  (or (alist-get 'name repo)
      (alist-get 'full_name repo)
      "unknown"))

(defun ip-forgejo--extract-tags (issue)
  "Extract tags from ISSUE labels and convert to Org tags format."
  (let* ((labels (alist-get 'labels issue))
         (label-names (when (and labels (listp labels))
                       (mapcar (lambda (label)
                                 (let ((name (alist-get 'name label)))
                                   (when name
                                     (replace-regexp-in-string 
                                      "[^A-Za-z0-9_@]" "_"
                                      (replace-regexp-in-string "/" "_" name)))))
                               labels))))
    (cl-remove-if #'null label-names)))

;;; ============================================================================
;;; Org Entry Formatting (now returns structured data)
;;; ============================================================================

(defun ip-forgejo--format-timestamp (iso-str)
  "Convert ISO8601 to Org timestamp."
  (when (and iso-str (not (string-empty-p iso-str)))
    (condition-case nil
        (format-time-string "%Y-%m-%d %a" (date-to-time iso-str))
      (error nil))))

(defun ip-forgejo--format-timestamp-with-time (iso-str)
  "Convert ISO8601 to Org timestamp with time."
  (when (and iso-str (not (string-empty-p iso-str)))
    (condition-case nil
        (format-time-string "%Y-%m-%d %a %H:%M" (date-to-time iso-str))
      (error nil))))

(defun ip-forgejo--format-entry-data (issue times issue-url)
  "Return plist with structured data for Org entry."
  (let* ((title (or (alist-get 'title issue) "Untitled"))
         (state (or (alist-get 'state issue) "open"))
         (todo (if (string= state "closed") "DONE" "TODO"))
         (body (or (alist-get 'body issue) ""))
         (repo (alist-get 'repository issue))
         (owner (ip-forgejo--extract-owner repo))
         (repo-name (ip-forgejo--extract-repo-name repo))
         (label-tags (ip-forgejo--extract-tags issue))
         (all-tags (seq-uniq (append label-tags (list owner repo-name))))
         (tags-str (if all-tags
                       (concat ":" (mapconcat #'identity all-tags ":") ":")
                     ""))
         (tags-part (if tags-str (concat "    " tags-str) ""))
         (heading (format "* %s %s%s" todo title tags-part))
         (total-time (if (and times (listp times))
                         (cl-reduce #'+ (mapcar (lambda (e) (or (alist-get 'time e) 0)) times)
                                    :initial-value 0)
                       0))
         (log-entries (when (and times (listp times) (> (length times) 0))
                        (cl-remove nil
                                   (mapcar
                                    (lambda (entry)
                                      (let* ((created (alist-get 'created entry))
                                             (duration (or (alist-get 'time entry) 0))
                                             (end-ts (condition-case nil
                                                       (date-to-time created)
                                                     (error nil))))
                                        (when (and end-ts (> duration 0))
                                          (let* ((start-ts (time-subtract end-ts (seconds-to-time duration)))
                                                 (start-str (format-time-string "[%Y-%m-%d %a %H:%M]" start-ts))
                                                 (end-str (format-time-string "[%Y-%m-%d %a %H:%M]" end-ts))
                                                 (h (floor duration 3600))
                                                 (m (/ (mod duration 3600) 60)))
                                            (format "CLOCK: %s--%s => %2d:%02d" start-str end-str h m)))))
                                    times))))
         (logbook-inner (when log-entries
                          (concat (string-join log-entries "\n") "\n")))
         (created-at (alist-get 'created_at issue))
         (updated-at (alist-get 'updated_at issue))
         (closed-at (alist-get 'closed_at issue))
         (due-date (alist-get 'due_date issue))
         (closed-str (when closed-at
                       (let ((ts (ip-forgejo--format-timestamp-with-time closed-at)))
                         (when ts (format "CLOSED: [%s]" ts)))))
         (scheduled-str (when created-at
                          (let ((ts (ip-forgejo--format-timestamp created-at)))
                            (when ts (format "SCHEDULED: <%s>" ts)))))
         (deadline-str (when due-date
                         (let ((ts (ip-forgejo--format-timestamp due-date)))
                           (when ts (format "DEADLINE: <%s>" ts)))))
         (planning-parts (cl-remove nil (list closed-str scheduled-str deadline-str)))
         (planning (when planning-parts
                     (concat "  " (string-join planning-parts " ") "\n")))
         (properties `(("FORGEJO_URL" . ,issue-url)
                       ("STATE" . ,state)
                       ("TIME" . ,(number-to-string total-time))
                       ("CREATED" . ,(or created-at ""))
                       ("UPDATED" . ,(or updated-at "")))))
    (list :heading heading
          :planning planning
          :logbook logbook-inner
          :body body
          :properties properties)))

;;; ============================================================================
;;; Org API Helpers for Insertion/Update
;;; ============================================================================

(defun ip-forgejo--remove-planning-lines ()
  "Remove all existing planning lines (CLOSED:/SCHEDULED:/DEADLINE:)."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (while (looking-at org-planning-line-re)
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (forward-line 1))))

(defun ip-forgejo--replace-drawer (drawer-name content)
  "Replace or insert drawer DRAWER-NAME with CONTENT (inner text only)."
  (save-excursion
    (org-back-to-heading t)
    (let ((drawer-re (format "^:%s:$" (upcase drawer-name))))
      (when (re-search-forward drawer-re (org-entry-end-position) t)
        (let ((start (line-beginning-position))
              (end (or (and (re-search-forward "^:END:$" (org-entry-end-position) t)
                            (line-beginning-position))
                       (org-entry-end-position))))
          (delete-region start end)))
      (when content
        (org-end-of-meta-data t)
        (insert (format ":%s:\n%s:END:\n" (upcase drawer-name) content))))))

(defun ip-forgejo--replace-body (content)
  "Replace body text with CONTENT, preserving any sub-headings."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (skip-chars-forward " \t\n")
    (let ((body-start (point))
          (body-end (or (save-excursion
                          (when (re-search-forward "^\\* " nil t)
                            (match-beginning 0)))
                        (point-max))))
      (when (< body-start body-end)
        (delete-region body-start body-end))
      (goto-char body-start)
      (when (and content (not (string-empty-p (string-trim content))))
        (insert (string-trim-right content))
        (unless (bolp) (insert "\n"))
        (insert "\n")))))

;;; ============================================================================
;;; Org Buffer Operations (now using Org API)
;;; ============================================================================

(defun ip-forgejo--find-entry-by-url (url)
  "Find Org heading with FORGEJO_URL property matching URL."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward "^[ \t]*:FORGEJO_URL:[ \t]+\\(.*\\)$" nil t)
        (when (string= (string-trim (match-string 1)) url)
          (org-back-to-heading t)
          (throw 'found (point))))
      nil)))

(defun ip-forgejo--insert-or-update (url data)
  "Insert new entry or update existing one using structured DATA and URL key."
  (let ((pos (ip-forgejo--find-entry-by-url url))
        result)
    (save-excursion
      (if pos
          (goto-char pos)
        (goto-char (point-max))
        (unless (bolp) (insert "\n")))
      (when (not pos)
        (insert (plist-get data :heading) "\n"))
      (org-back-to-heading t)
      (when pos
        (delete-region (line-beginning-position) (1+ (line-end-position)))
        (insert (plist-get data :heading) "\n"))
      ;; Stable Org ID
      (org-id-get-create)
      ;; Properties via Org API
      (dolist (pair (plist-get data :properties))
        (org-entry-put nil (car pair) (cdr pair)))
      ;; Planning line
      (ip-forgejo--remove-planning-lines)
      (when (plist-get data :planning)
        (org-back-to-heading t)
        (end-of-line)
        (insert "\n")
        (insert (plist-get data :planning)))
      ;; Logbook drawer
      (ip-forgejo--replace-drawer "LOGBOOK" (plist-get data :logbook))
      ;; Body text
      (ip-forgejo--replace-body (plist-get data :body))
      (setq result (if pos 'updated 'inserted)))
    result))

;;; ============================================================================
;;; Import Logic - Async Orchestration
;;; ============================================================================

(defun ip-forgejo--process-issue (issue state)
  "Process single ISSUE within import STATE."
  (let* ((repo (alist-get 'repository issue))
         (owner (ip-forgejo--extract-owner repo))
         (repo-name (ip-forgejo--extract-repo-name repo))
         (number (alist-get 'number issue))
         (issue-id (alist-get 'id issue))
         (config (ip-forgejo--config))
         (base-url (car config))
         (web-url (replace-regexp-in-string "/api/v1$" "" base-url))
         (issue-url (format "%s/%s/%s/issues/%d" web-url owner repo-name number))
         (times-url (format "%s/repos/%s/%s/issues/%d/times"
                            base-url owner repo-name number)))
    
    (ip-forgejo--log 'info "→ PROCESS: issue #%d (id=%s, %s/%s)" 
                     number issue-id owner repo-name)
    
    (ip-forgejo--api-async
     times-url
     (lambda (times)
       (let ((times-count (if (listp times) (length times) 0)))
         (ip-forgejo--log 'info "← RECEIVED: issue #%d has %d time entries" 
                          number times-count)
         (let ((data (ip-forgejo--format-entry-data issue times issue-url)))
           (ip-forgejo--log 'info "→ INSERT/UPDATE: issue #%d to buffer %s"
                            number (buffer-name (ip-forgejo-import-state-buffer state)))
           (condition-case err
               (with-current-buffer (ip-forgejo-import-state-buffer state)
                 (save-excursion
                   (let ((result (ip-forgejo--insert-or-update issue-url data)))
                     (cl-incf (ip-forgejo-import-state-processed state))
                     (when (eq result 'inserted)
                       (cl-incf (ip-forgejo-import-state-inserted state)))
                     (when (eq result 'updated)
                       (cl-incf (ip-forgejo-import-state-updated state)))
                     (ip-forgejo--log 'success "✓ DONE: issue #%d (%s)" number result)
                     (ip-forgejo--progress state)
                     (when (>= (ip-forgejo-import-state-processed state)
                               (ip-forgejo-import-state-total state))
                       (ip-forgejo--log 'success "ALL ISSUES PROCESSED - completing import")
                       (ip-forgejo--import-complete state)))))
             (error
              (ip-forgejo--log 'error "✗ FAILED to insert issue #%d: %s" number err)
              (cl-incf (ip-forgejo-import-state-processed state))
              (push (format "Issue #%d insert failed: %s" number err)
                    (ip-forgejo-import-state-errors state))
              (ip-forgejo--progress state)
              (when (>= (ip-forgejo-import-state-processed state)
                        (ip-forgejo-import-state-total state))
                (ip-forgejo--import-complete state)))))))
     (lambda (err)
       (ip-forgejo--log 'error "✗ FAILED to fetch times for issue #%d: %s" number err)
       (cl-incf (ip-forgejo-import-state-processed state))
       (push (format "Issue #%d times fetch failed: %s" number err)
             (ip-forgejo-import-state-errors state))
       (ip-forgejo--progress state)
       (when (>= (ip-forgejo-import-state-processed state)
                 (ip-forgejo-import-state-total state))
         (ip-forgejo--log 'warning "All issues attempted despite errors - completing import")
         (ip-forgejo--import-complete state))))))

(defun ip-forgejo--import-complete (state &optional forced)
  "Handle completion of import STATE. If FORCED, mention incomplete status."
  (let ((duration (time-subtract (current-time)
                                 (ip-forgejo-import-state-start-time state)))
        (buffer (ip-forgejo-import-state-buffer state))
        (processed (ip-forgejo-import-state-processed state))
        (total (ip-forgejo-import-state-total state)))
    
    (with-current-buffer buffer
      (save-buffer))
    
    (if forced
        (progn
          (ip-forgejo--log 'warning
                           "Import INCOMPLETE (forced): %d/%d processed, %d inserted, %d updated, %d errors (%.1fs)"
                           processed total
                           (ip-forgejo-import-state-inserted state)
                           (ip-forgejo-import-state-updated state)
                           (length (ip-forgejo-import-state-errors state))
                           (float-time duration))
          (message "Forgejo import INCOMPLETE: %d/%d processed (%d new, %d updated) - check *Forgejo Log*"
                   processed total
                   (ip-forgejo-import-state-inserted state)
                   (ip-forgejo-import-state-updated state)))
      
      (ip-forgejo--log 'success
                       "Import complete: %d inserted, %d updated, %d errors (%.1fs)"
                       (ip-forgejo-import-state-inserted state)
                       (ip-forgejo-import-state-updated state)
                       (length (ip-forgejo-import-state-errors state))
                       (float-time duration))
      
      (message "Forgejo import complete: %d new, %d updated in %.1fs"
               (ip-forgejo-import-state-inserted state)
               (ip-forgejo-import-state-updated state)
               (float-time duration)))
    
    (when (or forced (ip-forgejo-import-state-errors state))
      (display-buffer "*Forgejo Log*"))
    
    (when ip-forgejo--import-timeout-timer
      (cancel-timer ip-forgejo--import-timeout-timer)
      (setq ip-forgejo--import-timeout-timer nil))
    
    (setq ip-forgejo--import-state nil)))

;;; ============================================================================
;;; Public API
;;; ============================================================================

;;;###autoload
(defun ip-forgejo-import-my-issues ()
  "Import assigned issues asynchronously (non-blocking)."
  (interactive)
  
  (when ip-forgejo--import-state
    (if (yes-or-no-p "Import already in progress. Abort and restart? ")
        (setq ip-forgejo--import-state nil)
      (user-error "Import already in progress!")))
  
  (let* ((config (ip-forgejo--config))
         (base-url (car config))
         (user-url (format "%s/user" base-url)))
    
    (ip-forgejo--log 'info "Starting import from %s" ip-forgejo-current-instance)
    (message "Fetching issues from %s..." ip-forgejo-current-instance)
    
    (ip-forgejo--api-async
     user-url
     (lambda (user)
       (let ((username (alist-get 'login user))
             (open-url (format "%s/repos/issues/search?assigned=true&state=open" base-url))
             (closed-url (format "%s/repos/issues/search?assigned=true&state=closed" base-url))
             (all-issues nil))
         
         (ip-forgejo--log 'info "Fetching issues for user: %s" username)
         
         (ip-forgejo--api-async
          open-url
          (lambda (open-issues)
            (setq all-issues (append all-issues (if (listp open-issues) open-issues nil)))
            (ip-forgejo--log 'info "Found %d open issues" (length all-issues))
            
            (ip-forgejo--api-async
             closed-url
             (lambda (closed-issues)
               (setq all-issues (append all-issues (if (listp closed-issues) closed-issues nil)))
               (ip-forgejo--log 'info "Found %d closed issues" (length closed-issues))
               (ip-forgejo--log 'info "Total: %d issues" (length all-issues))
               
               (if (= (length all-issues) 0)
                   (message "No issues found to import")
                 
                 (setq ip-forgejo--import-state
                       (make-ip-forgejo-import-state
                        :buffer (current-buffer)
                        :issues all-issues
                        :processed 0
                        :total (length all-issues)
                        :inserted 0
                        :updated 0
                        :errors nil
                        :start-time (current-time)
                        :last-activity (current-time)))
                 
                 (when ip-forgejo--import-timeout-timer
                   (cancel-timer ip-forgejo--import-timeout-timer))
                 (setq ip-forgejo--import-timeout-timer
                       (run-with-timer 5 5 'ip-forgejo--check-stalled))
                 
                 (message "Processing %d issues asynchronously..." (length all-issues))
                 (dolist (issue all-issues)
                   (ip-forgejo--process-issue issue ip-forgejo--import-state))))
             
             (lambda (err)
               (ip-forgejo--log 'error "Failed to fetch closed issues: %s" err)
               (message "Error fetching closed issues: %s" err))))
          
          (lambda (err)
            (ip-forgejo--log 'error "Failed to fetch open issues: %s" err)
            (message "Error fetching open issues: %s" err)))))
     
     (lambda (err)
       (ip-forgejo--log 'error "Failed to get user info: %s" err)
       (message "Error getting user info: %s" err)))))

;;;###autoload
(defun ip-forgejo-abort-import ()
  "Abort current import operation."
  (interactive)
  (if ip-forgejo--import-state
      (progn
        (when ip-forgejo--import-timeout-timer
          (cancel-timer ip-forgejo--import-timeout-timer)
          (setq ip-forgejo--import-timeout-timer nil))
        (setq ip-forgejo--import-state nil)
        (message "Forgejo import aborted"))
    (message "No import in progress")))

;;;###autoload
(defun ip-forgejo-force-complete ()
  "Force completion of stalled import."
  (interactive)
  (if ip-forgejo--import-state
      (progn
        (ip-forgejo--log 'warning "Forcing import completion...")
        (ip-forgejo--import-complete ip-forgejo--import-state t))
    (message "No import in progress")))

;;;###autoload
(defun ip-forgejo-switch-instance (instance)
  "Switch to different Forgejo INSTANCE."
  (interactive
   (list (completing-read "Instance: "
                          (mapcar #'car ip-forgejo-instances)
                          nil t)))
  (setq ip-forgejo-current-instance instance)
  (message "Switched to Forgejo instance: %s" instance))

;;;###autoload
(defun ip-forgejo-show-log ()
  "Show Forgejo import log buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Forgejo Log*")))
    (with-current-buffer buf
      (goto-char (point-max)))
    (display-buffer buf)))

;;;###autoload
(defun ip-forgejo-debug-state ()
  "Show current import state for debugging."
  (interactive)
  (if ip-forgejo--import-state
      (let ((state ip-forgejo--import-state))
        (message "Import state: %d/%d processed, %d new, %d updated, %d errors"
                 (ip-forgejo-import-state-processed state)
                 (ip-forgejo-import-state-total state)
                 (ip-forgejo-import-state-inserted state)
                 (ip-forgejo-import-state-updated state)
                 (length (ip-forgejo-import-state-errors state)))
        (when (ip-forgejo-import-state-errors state)
          (message "Errors: %s" (ip-forgejo-import-state-errors state))))
    (message "No import in progress")))

;;; ============================================================================
;;; Minor Mode
;;; ============================================================================

(defvar ip-forgejo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f i") 'ip-forgejo-import-my-issues)
    (define-key map (kbd "C-c f s") 'ip-forgejo-switch-instance)
    (define-key map (kbd "C-c f a") 'ip-forgejo-abort-import)
    (define-key map (kbd "C-c f l") 'ip-forgejo-show-log)
    (define-key map (kbd "C-c f f") 'ip-forgejo-force-complete)
    (define-key map (kbd "C-c f d") 'ip-forgejo-debug-state)
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