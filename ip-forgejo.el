;;; ip-forgejo.el --- Forgejo issues integration for IP management system -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025 IP Management System
;; Author: IP Management System
;; Version: 2.1
;; Keywords: org, forgejo, issues, time-tracking, synchronization
;; Package-Requires: ((emacs "27.1") (org "9.0") (request "0.3") (json "1.4"))

;;; Commentary:

;; This module handles synchronization between Forgejo issues and Org-mode.
;;
;; Features:
;; - Import assigned Forgejo issues as Org-mode tasks (both open and closed)
;; - Synchronize issue status, deadlines, and time logs
;; - Two-way sync for status updates and time tracking
;; - Multi-instance support for different Forgejo servers
;; - Robust API handling with retry logic and caching
;; - Automatic connection cleanup to prevent API failures
;;
;; Usage:
;;   M-x ip-forgejo-import-my-issues        ; Import assigned issues
;;   M-x ip-forgejo-switch-instance         ; Switch Forgejo instance
;;   M-x ip-forgejo-push-current-entry      ; Push changes to Forgejo

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'org)
(require 'org-element)
(require 'time-date)
(require 'json)
(require 'subr-x)

;; External dependencies
(declare-function ip-debug-log "ip-debug" (level module message &rest args))
(declare-function org-clock-in "org-clock" ())
(declare-function org-clock-out "org-clock" ())

;; Variables from other modules
(defvar org-clock-start-time)

;; Fallback logging if ip-debug is not available
(condition-case nil
    (require 'ip-debug)
  (error
   (defun ip-debug-log (level module message &rest args)
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

   (defmacro ip-debug (module message &rest args)
     "Fallback debug macro that uses `ip-debug-log'."
     `(ip-debug-log 'info ,module ,message ,@args))))

;;; Customization

(defgroup ip-forgejo nil
  "Forgejo issues integration for IP management system."
  :group 'ip-core
  :prefix "ip-forgejo-")

(defcustom ip-forgejo-instances
  '(("work"    . (("base-url" . "https://git.company.com/api/v1")
                  ("token"    . "tkn_work_123")))
    ("home"    . (("base-url" . "https://git.home.org/api/v1")
                  ("token"    . "tkn_home_456")))
    ("client"  . (("base-url" . "http://git.maketv.internal/api/v1")
                  ("token"    . "52ba51cd10ba0250444d872e10ac1dd730cee076"))))
  "List of Forgejo instances configuration."
  :type '(alist :key-type string
                :value-type (alist :key-type string :value-type string))
  :group 'ip-forgejo)

(defcustom ip-forgejo-current-instance "client"
  "Default Forgejo instance to use for API calls."
  :type 'string
  :group 'ip-forgejo)

(defcustom ip-forgejo-cache-ttl 300
  "Cache TTL in seconds for API responses (default 5 minutes)."
  :type 'integer
  :group 'ip-forgejo)

(defcustom ip-forgejo-api-timeout 30
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'ip-forgejo)

(defcustom ip-forgejo-max-retries 3
  "Maximum number of retry attempts for failed API calls."
  :type 'integer
  :group 'ip-forgejo)

(defcustom ip-forgejo-auto-clear-cache t
  "Automatically clear cache when switching instances."
  :type 'boolean
  :group 'ip-forgejo)

(defcustom ip-forgejo-sync-on-save t
  "Automatically sync deadlines when saving Org files."
  :type 'boolean
  :group 'ip-forgejo)

;;; Variables

(defvar ip-forgejo--cache (make-hash-table :test 'equal)
  "Cache for API responses with timestamp tracking.")

(defvar ip-forgejo--sync-buffer-name "*Forgejo Sync Report*"
  "Name of the buffer used for sync reports.")

;;; Utility Functions

(defun ip-forgejo--current-config ()
  "Return (base-url . token) for current instance."
  (let* ((instance (assoc ip-forgejo-current-instance ip-forgejo-instances)))
    (unless instance
      (ip-debug-log 'error 'forgejo "Unknown Forgejo instance: %s"
                    ip-forgejo-current-instance)
      (user-error "Unknown Forgejo instance: %s" ip-forgejo-current-instance))
    (let ((config (cdr instance)))
      (cons (alist-get "base-url" config nil nil #'equal)
            (alist-get "token" config nil nil #'equal)))))

(defun ip-forgejo--clean-body (body)
  "Remove carriage returns and normalize line endings in BODY."
  (when body
    (replace-regexp-in-string "\r\n?" "\n" body)))

(defun ip-forgejo--ensure-list (value)
  "Convert VALUE to list if it's a vector or nil."
  (cond
   ((null value) '())
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun ip-forgejo--safe-get (alist key &optional default)
  "Safely get KEY from ALIST, returning DEFAULT if not found."
  (let ((value (alist-get key alist)))
    (if (and value (not (string-empty-p (format "%s" value))))
        value
      default)))

(defun ip-forgejo--format-org-timestamp (iso8601-str)
  "Convert ISO 8601 string to Org-mode timestamp format."
  (when (and iso8601-str (not (string-empty-p iso8601-str)))
    (let ((time (ignore-errors (date-to-time iso8601-str))))
      (when time
        (format-time-string "%Y-%m-%d %a" time)))))

;;; Property Management

(defun ip-forgejo--get-forgejo-url-from-current-entry ()
  "Extract FORGEJO_URL from current Org entry using multiple methods."
  (save-excursion
    (org-back-to-heading t)
    (let (url)
      ;; Method 1: Try org-entry-get (for standard properties)
      (setq url (org-entry-get nil "FORGEJO_URL"))
      
      ;; Method 2: Search in properties drawer
      (unless url
        (save-excursion
          (when (re-search-forward "^\\s-*:PROPERTIES:" 
                                  (save-excursion (org-end-of-subtree t) (point)) t)
            (when (re-search-forward "^\\s-*:FORGEJO_URL:\\s-+\\(.*\\)" 
                                    (save-excursion 
                                      (re-search-forward "^\\s-*:END:" (point-max) t) 
                                      (point)) t)
              (setq url (string-trim (match-string 1)))))))
      
      ;; Method 3: Search anywhere in the entry
      (unless url
        (save-excursion
          (let ((end (save-excursion (org-end-of-subtree t) (point))))
            (when (re-search-forward ":FORGEJO_URL:\\s-+\\([^\n]+\\)" end t)
              (setq url (string-trim (match-string 1)))))))
      
      url)))

(defun ip-forgejo--find-entry-by-url (issue-url)
  "Find Org heading by FORGEJO_URL property."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward "^\\*+ " nil t)
        (let ((heading-start (match-beginning 0)))
          (goto-char heading-start)
          (when (string-equal (ip-forgejo--get-forgejo-url-from-current-entry) issue-url)
            (throw 'found (point)))))
      nil)))

;;; Cache Management

(defun ip-forgejo--cache-put (url data)
  "Store DATA for URL with current timestamp."
  (puthash url (list :data data :timestamp (current-time)) ip-forgejo--cache))

(defun ip-forgejo--cache-get (url)
  "Get cached data for URL if not expired."
  (when-let ((entry (gethash url ip-forgejo--cache)))
    (let ((timestamp (plist-get entry :timestamp))
          (data (plist-get entry :data)))
      (if (< (time-to-seconds (time-subtract (current-time) timestamp))
             ip-forgejo-cache-ttl)
          data
        (remhash url ip-forgejo--cache)
        nil))))

(defun ip-forgejo--clear-cache-and-connections ()
  "Clear API cache and HTTP connection state."
  (clrhash ip-forgejo--cache)
  (when (boundp 'url-http-connection-cache)
    (clrhash url-http-connection-cache))
  (when (boundp 'request--curl-cookie-jar)
    (setq request--curl-cookie-jar nil))
  (ip-debug-log 'info 'forgejo "Cache and connections cleared"))

;;; Logging and Reporting

(defun ip-forgejo--log (level message &rest args)
  "Log MESSAGE with LEVEL to sync process buffer AND unified debug system."
  (let ((formatted-msg (apply #'format message args))
        (timestamp (format-time-string "[%H:%M:%S] "))
        (icon (pcase level
                ('info "ℹ️")
                ('success "✓")
                ('warning "⚠️")
                ('error "❌")
                (_ "?"))))
    ;; Log to unified debug system
    (ip-debug-log level 'forgejo "%s" formatted-msg)
    ;; Also log to specific sync process buffer for detailed reports
    (with-current-buffer (get-buffer-create ip-forgejo--sync-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "%s%s %s\n" timestamp icon formatted-msg))))))

;;; API Communication

(defun ip-forgejo--api-request (url &optional headers max-retries)
  "Send API request with retry logic and connection cleanup."
  (let* ((config (ip-forgejo--current-config))
         (token (cdr config))
         (auth-header `("Authorization" . ,(concat "token " token)))
         (all-headers (cons auth-header (or headers '())))
         (retries (or max-retries ip-forgejo-max-retries))
         (delay 1)
         result
         (request-backend 'sync))  ;; Force sync backend to avoid curl issues

  (ip-forgejo--log 'info "Request: GET %s (retries: %d)" url retries)

  (while (and (> retries 0) (not result))
    (condition-case err
        (progn
          ;; Process events to prevent Emacs hanging
          (while-no-input
            (let ((response
                   (request url
                    :type "GET"
                    :headers all-headers
                    :parser (lambda ()
                              (condition-case parse-err
                                  (json-parse-string (buffer-string)
                                                     :object-type 'alist
                                                     :array-type 'list
                                                     :null-object nil
                                                     :false-object :false)
                                (error
                                 (ip-forgejo--log 'error "JSON parse error: %s"
                                                  (error-message-string parse-err))
                                 nil)))
                    :sync t
                    :timeout ip-forgejo-api-timeout
                    :status-code 'success
                    :error (cl-function 
                            (lambda (&key error-thrown &allow-other-keys)
                              (ip-forgejo--log 'error "Request error: %s" error-thrown)))
                    :complete (lambda (&rest _)
                                ;; Force cleanup
                                (setq url-http-connection-cache nil)))))
              (let ((response-data (request-response-data response))
                    (status (request-response-status-code response)))
                
                (if (and (>= status 200) (< status 300))
                    (progn
                      (ip-forgejo--log 'success "Response: %d bytes"
                                       (length (prin1-to-string response-data)))
                      (setq result response-data))
                  (progn
                    (ip-forgejo--log 'error "HTTP %d for %s" status url)
                    (when (> retries 1)
                      (ip-forgejo--log 'warning "Retrying in %d seconds..." delay)
                      (sit-for delay)  ;; Use sit-for instead of sleep-for
                      (setq delay (* delay 2))))))))
          ;; If while-no-input was aborted, handle it
          (when (null result)
            (ip-forgejo--log 'warning "Request aborted by user input")
            (setq retries 0)))
      
      (error
       (ip-forgejo--log 'error "Request failed: %s" (error-message-string err))
       (when (> retries 1)
         (ip-forgejo--log 'warning "Clearing connections and retrying...")
         (ip-forgejo--clear-cache-and-connections)
         (sit-for delay)  ;; Use sit-for instead of sleep-for
         (setq delay (* delay 2)))))

    (setq retries (1- retries)))

  (unless result
    (ip-forgejo--log 'error "All retry attempts failed for %s" url))

  result))


(defun ip-forgejo--api (url &optional headers)
  "Send GET request to Forgejo API at URL with caching and error handling."
  (let ((cached (and (string-match-p "/issues" url)
                     (ip-forgejo--cache-get url))))
    (if cached
        (progn
          (ip-forgejo--log 'info "Cache hit: %s" url)
          cached)
      (let ((result (ip-forgejo--api-request url headers)))
        (when (and result (string-match-p "/issues" url))
          (ip-forgejo--cache-put url result))
        result))))

;;; Issue Status and Conversion

(defun ip-forgejo--org-todo-state (state)
  "Convert Forgejo issue STATE to Org-mode TODO keyword."
  (pcase state
    ("open" "TODO")
    ("closed" "DONE")
    (_ "TODO")))

(defun ip-forgejo--forgejo-state (todo-keyword)
  "Convert Org-mode TODO-KEYWORD to Forgejo issue state."
  (pcase todo-keyword
    ("TODO" "open")
    ("DONE" "closed")
    (_ "open")))

;;; Time Log Processing

(defun ip-forgejo--format-logbook (entries)
  "Format a list of time log ENTRIES into Org-mode LOGBOOK CLOCK lines."
  (if (null entries)
      ""
    (let ((formatted-entries
           (cl-remove nil
                      (mapcar
                       (lambda (entry)
                         (let* ((created-str (alist-get 'created entry))
                                (duration (or (alist-get 'time entry) 0))
                                (ts (and created-str
                                         (not (string-empty-p created-str))
                                         (condition-case nil
                                             (date-to-time created-str)
                                           (error nil))))
                                (end-time (and ts (> duration 0)
                                             (time-add ts (seconds-to-time duration))))
                                (start-str (and ts (format-time-string "[%Y-%m-%d %a %H:%M]" ts)))
                                (end-str (and end-time (format-time-string "[%Y-%m-%d %a %H:%M]" end-time)))
                                (h (/ duration 3600))
                                (m (/ (% duration 3600) 60)))
                           (when (and start-str end-str (> duration 0))
                             (format "CLOCK: %s--%s => %02d:%02d" start-str end-str h m))))
                       (ip-forgejo--ensure-list entries)))))
      (if formatted-entries
          (string-join formatted-entries "\n")
        ""))))

;;; Issue Entry Formatting

(defun ip-forgejo--format-entry (issue times)
  "Format a single ISSUE with TIMES into an Org heading."
  (let* ((title (or (alist-get 'title issue) "Untitled Issue"))
         (number (or (alist-get 'number issue) 0))
         (state (or (alist-get 'state issue) "open"))
         (todo (ip-forgejo--org-todo-state state))
         (labels (ip-forgejo--ensure-list (alist-get 'labels issue)))
         (repo-data (alist-get 'repository issue))
         (owner-data (alist-get 'owner repo-data))
         (owner-name (cond
                      ((stringp owner-data) owner-data)
                      ((and owner-data (alist-get 'login owner-data))
                       (alist-get 'login owner-data))
                      (t "unknown")))
         (repo-name (or (alist-get 'name repo-data) "unknown"))
         (id (or (alist-get 'id issue) 0))
         (body (ip-forgejo--clean-body (or (alist-get 'body issue) "")))
         (total-time (cl-reduce #'+ (mapcar (lambda (e) (or (alist-get 'time e) 0))
                                            (ip-forgejo--ensure-list times))
                                :initial-value 0))
         (logbook (ip-forgejo--format-logbook times))
         (org-id (org-id-new))
         ;; Generate Forgejo issue URL
         (base-url (car (ip-forgejo--current-config)))
         (web-url (replace-regexp-in-string "/api/v1$" "" base-url))
         (issue-url (format "%s/%s/%s/issues/%s" web-url owner-name repo-name number))
         ;; Extract dates
         (created (alist-get 'created_at issue))
         (due-date (alist-get 'deadline issue))
         ;; Format tags
         (label-tags (mapcar (lambda (lbl)
                              (replace-regexp-in-string "[^A-Za-z0-9_]+" "_"
                                                       (or (alist-get 'name lbl) "")))
                             labels))
         (all-tags (seq-uniq (append label-tags (list owner-name repo-name))))
         (tags-str (if all-tags
                      (concat "    :" (mapconcat 'identity all-tags ":") ":")
                    ""))
         ;; Format timestamps
         (scheduled-str (when created
                         (let ((timestamp (ip-forgejo--format-org-timestamp created)))
                           (when timestamp
                             (format "SCHEDULED: <%s>" timestamp)))))
         (deadline-str (when due-date
                        (let ((timestamp (ip-forgejo--format-org-timestamp due-date)))
                          (when timestamp
                            (format "DEADLINE: <%s>" timestamp)))))
         ;; Build properties
         (properties-str (format ":PROPERTIES:\n:ID: %s\n:FORGEJO_URL: %s\n:STATE: %s\n:REPO: %s\n:TIME: %d\n:ID_ISSUE: %s\n:END:"
                                 org-id issue-url state repo-name total-time id))
         (logbook-block (if (string-empty-p logbook)
                            ""
                          (format ":LOGBOOK:\n%s\n:END:" logbook))))

    (format "* %s %s%s\n%s\n%s\n%s\n%s\n\n%s"
            todo title tags-str
            (or scheduled-str "")
            (or deadline-str "")
            properties-str
            logbook-block
            body)))

;;; Issue Management

(defun ip-forgejo--replace-or-insert-entry (issue-url entry)
  "Find Org heading by FORGEJO_URL and replace its subtree with ENTRY."
  (with-current-buffer (current-buffer)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (buffer-undo-list t)
          modified-pos)

      ;; Reset cache before changes
      (org-element-cache-reset 'force)
      (setq org-element--cache nil)
      (setq org-element--cache-sync-timer nil)
      (when (timerp org-element--cache-sync-timer)
        (cancel-timer org-element--cache-sync-timer)
        (setq org-element--cache-sync-timer nil))

      ;; Find and update entry
      (let ((existing-pos (ip-forgejo--find-entry-by-url issue-url)))
        (if existing-pos
            (progn
              (goto-char existing-pos)
              (let ((beg (point))
                    (end (save-excursion
                           (condition-case nil
                               (org-end-of-subtree t t)
                             (error
                              (if (re-search-forward org-heading-regexp nil t)
                                  (goto-char (match-beginning 0))
                                (goto-char (point-max))))
                           (point))))
                (delete-region beg end)
                (insert entry "\n")
                (setq modified-pos beg)
                (ip-forgejo--log 'debug "Updated entry for %s" issue-url)))
          (progn
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (let ((insert-pos (point)))
              (insert entry "\n")
              (setq modified-pos insert-pos))
            (ip-forgejo--log 'debug "Inserted new entry for %s" issue-url))))

      ;; Log result
      (if (and modified-pos (>= modified-pos (point-min)) (<= modified-pos (point-max)))
          (ip-forgejo--log 'success "%s entry: %s"
                           (if (ip-forgejo--find-entry-by-url issue-url) "Updated" "Inserted")
                           issue-url)
        (ip-forgejo--log 'warning "Finished processing entry for %s" issue-url))))))

;;; Issue Synchronization

(defun ip-forgejo--push-issue (issue-url title body state &optional repo-owner repo-name)
  "Update remote Forgejo issue with new STATE, BODY, and TITLE."
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (token (cdr config))
         ;; Extract info from URL
         (url-parts (split-string issue-url "/" t))
         (owner (or repo-owner (nth 2 url-parts)))
         (repo (or repo-name (nth 3 url-parts)))
         (issue-number (string-to-number (car (last url-parts))))
         (api-url (format "%s/repos/%s/%s/issues/%d" base-url owner repo issue-number))
         (patch-data `((title . ,title)
                       (body . ,body)
                       (state . ,state))))

    (unless (and owner repo)
      (ip-forgejo--log 'error "Cannot determine repository info from URL %s" issue-url)
      (error "Cannot determine repository info from URL %s" issue-url))

    (ip-forgejo--log 'info "Pushing update to issue %s" issue-url)

    (request api-url
      :type "PATCH"
      :headers `(("Authorization" . ,(concat "token " token))
                 ("Content-Type" . "application/json"))
      :data (json-serialize patch-data)
      :sync t
      :success (cl-function (lambda (&key &allow-other-keys)
                              (ip-forgejo--log 'success "Issue %s updated" issue-url)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (ip-forgejo--log 'error "Failed to update issue %s: %s"
                                             api-url error-thrown))))))

(defun ip-forgejo--push-deadline (issue-url &optional repo-owner repo-name)
  "Push DEADLINE from current Org entry to Forgejo issue."
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (token (cdr config))
         ;; Extract info from URL
         (url-parts (split-string issue-url "/" t))
         (owner (or repo-owner (nth -4 url-parts)))
         (repo (or repo-name (nth -3 url-parts)))
         (issue-number (string-to-number (car (last url-parts))))
         (api-url (format "%s/repos/%s/%s/issues/%d" base-url owner repo issue-number))
         (deadline-str (org-entry-get nil "DEADLINE"))
         (deadline-iso8601 (when deadline-str
                             (format-time-string "%FT%T%z"
                                                 (org-time-string-to-time deadline-str)))))

    (unless (and owner repo)
      (ip-forgejo--log 'error "Cannot determine repository info from URL %s" issue-url)
      (error "Cannot determine repository info from URL %s" issue-url))

    (when deadline-iso8601
      (let ((patch-data `((deadline . ,deadline-iso8601))))
        (ip-forgejo--log 'info "Pushing deadline to issue %s: %s"
                         issue-url deadline-iso8601)

        (request api-url
          :type "PATCH"
          :headers `(("Authorization" . ,(concat "token " token))
                     ("Content-Type" . "application/json"))
          :data (json-serialize patch-data)
          :sync t
          :success (cl-function (lambda (&key &allow-other-keys)
                                  (ip-forgejo--log 'success
                                                   "Deadline for issue %s updated"
                                                   issue-url)))
          :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                                (ip-forgejo--log 'error
                                                 "Failed to update deadline for issue %s: %s"
                                                 issue-url error-thrown))))))))

(defun ip-forgejo--add-time-entry (issue-url time-seconds &optional repo-owner repo-name)
  "Log time entry to Forgejo issue."
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (token (cdr config))
         ;; Extract info from URL
         (url-parts (split-string issue-url "/" t))
         (owner (or repo-owner (nth -4 url-parts)))
         (repo (or repo-name (nth -3 url-parts)))
         (issue-number (string-to-number (car (last url-parts))))
         (api-url (format "%s/repos/%s/%s/issues/%d/times"
                         base-url owner repo issue-number))
         (post-data `((created . ,(format-time-string "%FT%T%z"))
                     (time . ,time-seconds))))

    (unless (and owner repo)
      (ip-forgejo--log 'error "Cannot determine repository info from URL %s" issue-url)
      (error "Cannot determine repository info from URL %s" issue-url))

    (request api-url
      :type "POST"
      :headers `(("Authorization" . ,(concat "token " token))
                 ("Content-Type" . "application/json"))
      :data (json-serialize post-data)
      :sync t
      :success (cl-function (lambda (&key &allow-other-keys)
                              (ip-forgejo--log 'success "Time logged for issue %s"
                                               issue-url)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (ip-forgejo--log 'error "Failed to log time for issue %s: %s"
                                             issue-url error-thrown))))))

;;; Hook Functions

(defun ip-forgejo--on-save-hook ()
  "Push deadline and state if current entry is a Forgejo issue."
  (when (and ip-forgejo-sync-on-save
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (let ((forgejo-url (ip-forgejo--get-forgejo-url-from-current-entry)))
        (when forgejo-url
          (ip-debug-log 'info 'forgejo "Auto-syncing deadline for issue %s"
                        forgejo-url)
          (ip-forgejo--push-deadline forgejo-url))))))

;;; Interactive Commands

;;;###autoload
(defun ip-forgejo-switch-instance ()
  "Switch current Forgejo instance."
  (interactive)
  (let ((names (mapcar 'car ip-forgejo-instances)))
    (setq ip-forgejo-current-instance
          (completing-read "Switch to instance: " names nil t))
    (when ip-forgejo-auto-clear-cache
      (ip-forgejo--clear-cache-and-connections))
    (ip-debug-log 'info 'forgejo "Switched to instance: %s"
                  ip-forgejo-current-instance)))

;;;###autoload
(defun ip-forgejo-clear-cache ()
  "Manually clear Forgejo API cache and connections."
  (interactive)
  (ip-forgejo--clear-cache-and-connections)
  (message "Forgejo cache and connections cleared"))

;;;###autoload
(defun ip-forgejo-validate-property-syntax ()
  "Check if FORGEJO_URL property has correct syntax in current entry."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((url (ip-forgejo--get-forgejo-url-from-current-entry)))
      (if url
          (progn
            (message "✓ FORGEJO_URL found: %s" url)
            (if (string-match-p "https?://" url)
                (message "✓ URL syntax appears valid")
              (message "⚠ URL may have syntax issues")))
        (message "✗ FORGEJO_URL not found in current entry")))))

;;;###autoload
(defun ip-forgejo-debug-current-entry ()
  "Debug information about current Org entry for Forgejo integration."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((forgejo-url (ip-forgejo--get-forgejo-url-from-current-entry))
          (todo-state (org-get-todo-state))
          (title (nth 4 (org-heading-components))))
      
      (message "=== Forgejo Debug Info ===")
      (message "Title: %s" title)
      (message "TODO state: %s" todo-state)
      (message "FORGEJO_URL: %s" forgejo-url)
      
      (when forgejo-url
        (let ((url-parts (split-string forgejo-url "/" t)))
          (message "URL parts: %s" url-parts)
          (when (>= (length url-parts) 4)
            (message "Owner: %s, Repo: %s, Issue: %s"
                     (nth -4 url-parts) (nth -3 url-parts) (nth -1 url-parts)))))
      
      (message "========================"))))

;;;###autoload
(defun ip-forgejo-import-my-issues (&optional instance)
  "Import assigned issues from current or specified Forgejo INSTANCE.
Imports both open and closed issues."
  (interactive)
  (when instance
    (setq ip-forgejo-current-instance instance))

  (ip-forgejo--log 'info "Starting import from instance: %s"
                   ip-forgejo-current-instance)

  ;; Clear cache before starting
  (ip-forgejo--clear-cache-and-connections)

  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (user-url (format "%s/user" base-url))
         (user-data (ip-forgejo--api user-url)))

    (unless user-data
      (ip-forgejo--log 'error "Failed to get user information")
      (error "Failed to get user information"))

    (let* ((username (alist-get 'login user-data))
           ;; Get both open and closed issues
           (open-issues-url (format "%s/repos/issues/search?assigned=true&state=open" base-url))
           (closed-issues-url (format "%s/repos/issues/search?assigned=true&state=closed" base-url))
           (all-issues '())
           (total-imported 0)
           (total-updated 0))

      ;; Fetch open issues with progress
      (ip-forgejo--log 'info "Fetching open issues...")
      (let ((open-issues-data (ip-forgejo--api open-issues-url)))
        (when open-issues-data
          (setq all-issues (append all-issues (ip-forgejo--ensure-list open-issues-data)))
          (ip-forgejo--log 'info "Found %d open issues" 
                           (length (ip-forgejo--ensure-list open-issues-data))))
        ;; Process pending events
        (sit-for 0.01))

      ;; Fetch closed issues with progress  
      (ip-forgejo--log 'info "Fetching closed issues...")
      (let ((closed-issues-data (ip-forgejo--api closed-issues-url)))
        (when closed-issues-data
          (setq all-issues (append all-issues (ip-forgejo--ensure-list closed-issues-data)))
          (ip-forgejo--log 'info "Found %d closed issues"
                           (length (ip-forgejo--ensure-list closed-issues-data))))
        (sit-for 0.01))

      (unless all-issues
        (ip-forgejo--log 'error "Failed to get issues data")
        (error "Failed to get issues data"))

      (ip-forgejo--log 'info "Processing %d total issues for user %s"
                       (length all-issues) username)

      ;; Process each issue with progress
      (dolist (issue all-issues)
        (let* ((issue-id (alist-get 'id issue))
               (repo-data (alist-get 'repository issue))
               (owner-data (alist-get 'owner repo-data))
               (owner-name (if (stringp owner-data)
                               owner-data
                             (alist-get 'login owner-data)))
               (repo-name (alist-get 'name repo-data))
               (issue-number (alist-get 'number issue))
               (state (alist-get 'state issue))
               ;; Generate issue URL first
               (web-url (replace-regexp-in-string "/api/v1$" "" base-url))
               (issue-url (format "%s/%s/%s/issues/%s"
                                  web-url owner-name repo-name issue-number)))

          (ip-forgejo--log 'info "Processing %s issue %s" state issue-id)

          ;; Only fetch times for existing entries to reduce API calls
          (let* ((existing-pos (ip-forgejo--find-entry-by-url issue-url))
                 (times-url (when existing-pos
                              (format "%s/repos/%s/%s/issues/%d/times"
                                      base-url owner-name repo-name issue-number)))
                 (times-data (when times-url (ip-forgejo--api times-url)))
                 (times (ip-forgejo--ensure-list times-data))
                 ;; Format the entry
                 (entry (ip-forgejo--format-entry issue times)))

            (ip-forgejo--replace-or-insert-entry issue-url entry)

            (if existing-pos
                (cl-incf total-updated)
              (cl-incf total-imported)))

          ;; Process events and show progress periodically
          (when (= (% (cl-incf total-processed) 5) 0)
            (message "Forgejo import progress: %d/%d issues processed..." 
                     total-processed (length all-issues))
            (sit-for 0.001))  ;; Allow event processing

          ;; Small delay to prevent overwhelming the server
          (sit-for 0.05)))

      (ip-forgejo--log 'success "Import completed: %d new, %d updated"
                       total-imported total-updated)

      ;; Display sync report
      (with-current-buffer (get-buffer-create ip-forgejo--sync-buffer-name)
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert "\n" (make-string 50 ?=) "\n")
          (insert (format "IMPORT SUMMARY [%s]\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (format "Instance: %s\n" ip-forgejo-current-instance))
          (insert (format "User: %s\n" username))
          (insert (format "Total issues processed: %d\n" (length all-issues)))
          (insert (format "New entries: %d\n" total-imported))
          (insert (format "Updated entries: %d\n" total-updated))
          (insert (make-string 50 ?=) "\n"))
        (display-buffer (current-buffer)))

      (message "Forgejo import completed: %d new, %d updated issues"
               total-imported total-updated))))

;;;###autoload
(defun ip-forgejo-push-current-entry ()
  "Push current Org entry state and deadline back to Forgejo."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((forgejo-url (ip-forgejo--get-forgejo-url-from-current-entry))
          (todo-state (org-get-todo-state))
          (title (nth 4 (org-heading-components)))
          (body (save-excursion
                  (org-end-of-meta-data t)
                  (let ((content-start (point))
                        (content-end (save-excursion
                                       (org-end-of-subtree t)
                                       (point))))
                    (buffer-substring-no-properties content-start content-end)))))

      (unless forgejo-url
        (ip-forgejo--log 'error "Current entry is not a Forgejo issue")
        (error "Current entry is not a Forgejo issue. FORGEJO_URL not found."))

      (let ((forgejo-state (ip-forgejo--forgejo-state todo-state)))
        (ip-forgejo--log 'info "Pushing update to %s: state=%s" forgejo-url forgejo-state)
        (ip-forgejo--push-issue forgejo-url title body forgejo-state)
        (ip-forgejo--push-deadline forgejo-url)
        (message "Pushed changes to Forgejo issue: %s" forgejo-url)))))

;;;###autoload
(defun ip-forgejo-clock-in-and-log ()
  "Clock in to current Forgejo issue and log time when clocking out."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((forgejo-url (ip-forgejo--get-forgejo-url-from-current-entry)))
      (unless forgejo-url
        (ip-forgejo--log 'error "Current entry is not a Forgejo issue")
        (error "Current entry is not a Forgejo issue"))

      (org-clock-in)

      ;; Add advice to org-clock-out to log time to Forgejo
      (advice-add 'org-clock-out :after
                  (lambda ()
                    (when (and (ip-forgejo--get-forgejo-url-from-current-entry)
                               (boundp 'org-clock-start-time)
                               org-clock-start-time)
                      (let* ((end-time (current-time))
                             (duration (time-to-seconds
                                       (time-subtract end-time org-clock-start-time)))
                             (issue-url (ip-forgejo--get-forgejo-url-from-current-entry)))
                        (when (> duration 60) ; Only log if more than 1 minute
                          (ip-forgejo--add-time-entry issue-url (round duration))))))
                  '((name . ip-forgejo-auto-log))))))

;;;###autoload
(defun ip-forgejo-refresh-current-issue ()
  "Refresh current Forgejo issue from server."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((forgejo-url (ip-forgejo--get-forgejo-url-from-current-entry)))

      (unless forgejo-url
        (ip-forgejo--log 'error "Current entry is not a Forgejo issue")
        (error "Current entry is not a Forgejo issue"))

      ;; Extract repository info from URL
      (let* ((url-parts (split-string forgejo-url "/" t))
             (owner (nth -4 url-parts))
             (repo (nth -3 url-parts))
             (issue-number (nth -1 url-parts))
             (config (ip-forgejo--current-config))
             (base-url (car config))
             ;; API URLs
             (issue-api-url (format "%s/repos/%s/%s/issues/%s"
                                   base-url owner repo issue-number))
             (times-api-url (format "%s/repos/%s/%s/issues/%s/times"
                                   base-url owner repo issue-number)))

        ;; Clear cache for this issue
        (remhash issue-api-url ip-forgejo--cache)
        (remhash times-api-url ip-forgejo--cache)

        ;; Fetch fresh data
        (let* ((issue-data (ip-forgejo--api issue-api-url))
               (times-data (ip-forgejo--api times-api-url))
               (times (ip-forgejo--ensure-list times-data)))

          (unless issue-data
            (ip-forgejo--log 'error "Failed to refresh issue data")
            (error "Failed to refresh issue data"))

          ;; Update the entry
          (let ((entry (ip-forgejo--format-entry issue-data times)))
            (ip-forgejo--replace-or-insert-entry forgejo-url entry)
            (ip-forgejo--log 'success "Refreshed issue: %s" forgejo-url)
            (message "Issue refreshed from server")))))))

;;; Setup and Teardown

;;;###autoload
(defun ip-forgejo-setup ()
  "Setup Forgejo integration hooks and keybindings."
  (interactive)
  ;; Add save hook for automatic deadline sync
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'ip-forgejo--on-save-hook nil t)))
  (ip-debug-log 'info 'forgejo "Forgejo integration setup completed"))

;;;###autoload
(defun ip-forgejo-teardown ()
  "Remove Forgejo integration hooks and clean up."
  (interactive)
  ;; Remove hooks
  (remove-hook 'org-mode-hook
               (lambda ()
                 (remove-hook 'after-save-hook 'ip-forgejo--on-save-hook t)))
  ;; Remove advice
  (advice-remove 'org-clock-out '((name . ip-forgejo-auto-log)))
  ;; Clear cache
  (ip-forgejo--clear-cache-and-connections)
  (ip-debug-log 'info 'forgejo "Forgejo integration teardown completed"))

;;; Minor Mode

(defvar ip-forgejo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f i") 'ip-forgejo-import-my-issues)
    (define-key map (kbd "C-c f s") 'ip-forgejo-switch-instance)
    (define-key map (kbd "C-c f p") 'ip-forgejo-push-current-entry)
    (define-key map (kbd "C-c f r") 'ip-forgejo-refresh-current-issue)
    (define-key map (kbd "C-c f c") 'ip-forgejo-clock-in-and-log)
    (define-key map (kbd "C-c f d") 'ip-forgejo-debug-current-entry)
    (define-key map (kbd "C-c f v") 'ip-forgejo-validate-property-syntax)
    (define-key map (kbd "C-c f C") 'ip-forgejo-clear-cache)
    map)
  "Keymap for `ip-forgejo-mode'.")

;;;###autoload
(define-minor-mode ip-forgejo-mode
  "Minor mode for Forgejo integration with Org-mode.

\\{ip-forgejo-mode-map}"
  :lighter " Forgejo"
  :keymap ip-forgejo-mode-map
  :group 'ip-forgejo
  (if ip-forgejo-mode
      (ip-forgejo-setup)
    (ip-forgejo-teardown)))

;;; Emergency recovery function

;;;###autoload
(defun ip-forgejo-abort-operation ()
  "Emergency function to abort any hanging Forgejo operations."
  (interactive)
  (ip-forgejo--clear-cache-and-connections)
  (setq url-http-connection-cache nil)
  (when (boundp 'request--curl-cookie-jar)
    (setq request--curl-cookie-jar nil))
  (cancel-function-timers 'ip-forgejo--api-request)
  (message "Forgejo operations aborted and cache cleared"))

;;; Add keybinding for emergency abort
(define-key ip-forgejo-mode-map (kbd "C-c f a") 'ip-forgejo-abort-operation)

;;; Debugging function

;;;###autoload
(defun ip-forgejo-test-connection ()
  "Test connection to current Forgejo instance."
  (interactive)
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (user-url (format "%s/user" base-url)))
    (message "Testing connection to %s..." base-url)
    (let ((result (ip-forgejo--api user-url)))
      (if result
          (message "✓ Connection successful! User: %s" (alist-get 'login result))
        (message "✗ Connection failed!")))))

;;; Provide

(provide 'ip-forgejo)

;;; ip-forgejo.el ends here