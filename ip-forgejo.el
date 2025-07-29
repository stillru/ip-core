;;; ip-forgejo.el --- Org importer from Forgejo issues -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles synchronization between Forgejo issues and Org-mode.
;; It fetches assigned issues, their time logs, and comments, formats them as Org entries,
;; and inserts or updates them in the current buffer.
;; Supports two-way sync for status, time, body, and deadlines.
;; Designed for multi-client reporting workflows.

;;; Code:

(require 'request)
(require 'org)
(require 'org-element)
(require 'time-date)

;; Try to load ip-debug, provide fallbacks if not available
(condition-case nil
    (require 'ip-debug)
  (error
   ;; Define fallback functions if ip-debug is not available
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
     "Fallback debug macro that uses `message'."
     `(ip-debug-log 'info ,module ,message ,@args))))

(defgroup ip-forgejo nil
  "Synchronization between local Org files and Forgejo issues."
  :group 'ip-core)

(defcustom ip-forgejo-instances
  '(("work"    . (("base-url" . "https://git.company.com/api/v1")
                  ("token"    . "tkn_work_123")))
    ("home"    . (("base-url" . "https://git.home.org/api/v1")
                  ("token"    . "tkn_home_456")))
    ("client"  . (("base-url" . "http://git.maketv.internal/api/v1")
                  ("token"    . "52ba51cd10ba0250444d872e10ac1dd730cee076"))))
  "List of Forgejo instances: ((NAME . ((\"base-url\" . URL) (\"token\" . TOKEN)))...)"
  :type '(alist :key-type string :value-type (alist :key-type string :value-type string))
  :group 'ip-forgejo)

(defcustom ip-forgejo-current-instance "client"
  "Default Forgejo instance to use."
  :type 'string
  :group 'ip-forgejo)

(defvar ip-forgejo--cache (make-hash-table :test 'equal)
  "Cache for API responses.")

(defun ip-forgejo--current-config ()
  "Return (base-url . token) for current instance."
  (let* ((instance (assoc ip-forgejo-current-instance ip-forgejo-instances)))
    (unless instance
      (ip-debug-log 'error 'forgejo "Unknown Forgejo instance: %s" ip-forgejo-current-instance)
      (user-error "Unknown Forgejo instance: %s" ip-forgejo-current-instance))
    (let ((config (cdr instance)))
      (cons (alist-get "base-url" config nil nil #'equal)
            (alist-get "token" config nil nil #'equal)))))

;;;###autoload
(defun ip-forgejo-switch-instance ()
  "Switch current Forgejo instance."
  (interactive)
  (let ((names (mapcar 'car ip-forgejo-instances)))
    (setq ip-forgejo-current-instance
          (completing-read "Switch to instance: " names nil t))
    (ip-debug-log 'info 'forgejo "Switched to instance: %s" ip-forgejo-current-instance)))

(defun ip-forgejo--clean-body (body)
  "Remove ^M and normalize line endings."
  (when body
    (replace-regexp-in-string "\r\n?" "\n" body)))

(defun ip-forgejo--ensure-list (value)
  "Convert VALUE to list if it's a vector or nil."
  (cond
   ((null value) '())
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

;; Keep the specific Forgejo sync process buffer for detailed reports
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
    (with-current-buffer (get-buffer-create "*Forgejo Sync Report*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "%s%s %s\n" timestamp icon formatted-msg))))))

(defun ip-forgejo--format-org-timestamp (iso8601-str)
  "Convert ISO 8601 string to Org-mode timestamp format."
  (when iso8601-str
    (let ((time (ignore-errors (date-to-time iso8601-str))))
      (when time
        (format-time-string "%Y-%m-%d %a" time)))))

(defun ip-forgejo--safe-get (alist key &optional default)
  "Safely get KEY from ALIST, returning DEFAULT if not found."
  (let ((value (alist-get key alist)))
    (if (and value (not (string-empty-p (format "%s" value))))
        value
      default)))

(defun ip-forgejo--api (url &optional headers)
  "Send a synchronous GET request to Forgejo API at URL."
  (let* ((config (ip-forgejo--current-config))
         (token (cdr config))
         (auth-header `("Authorization" . ,(concat "token " token)))
         (all-headers (cons auth-header (or headers '())))
         (cached (gethash url ip-forgejo--cache)))
    (when cached
      (ip-forgejo--log 'info "Cache hit: %s" url)
      (cl-return-from ip-forgejo--api cached))
    (ip-forgejo--log 'info "Request: GET %s" url)
    (condition-case err
        (let* ((result
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
                                      (ip-forgejo--log 'error "JSON parse error: %s" (error-message-string parse-err))
                                      nil)))
                         :sync t
                         :timeout 30))
               (response-data (request-response-data result))
               (status (request-response-status-code result)))
          (if (and (>= status 200) (< status 300))
              (progn
                (ip-forgejo--log 'success "Response: %d bytes" (length (prin1-to-string response-data)))
                (puthash url response-data ip-forgejo--cache)
                response-data)
            (progn
              (ip-forgejo--log 'error "HTTP %d: %s" status (buffer-string))
              nil)))
      (error
       (ip-forgejo--log 'error "Request failed: %s" (error-message-string err))
       nil))))

(defun ip-forgejo--org-todo-state (state)
  "Convert Forgejo issue STATE to Org-mode TODO keyword."
  (pcase state
    ("open" "TODO")
    ("closed" "DONE")
    (_ "TODO")))

(defun ip-forgejo--format-logbook (entries)
  "Format a list of time log ENTRIES into Org-mode :LOGBOOK: CLOCK lines."
  (if (null entries)
      ""
    (let ((formatted-entries
           (cl-remove nil
                      (mapcar
                       (lambda (entry)
                         (let* ((created-str (alist-get 'created entry))
                                (duration (alist-get 'time entry 0))
                                (ts (and created-str
                                         (condition-case nil
                                             (date-to-time created-str)
                                           (error nil))))
                                (end-time (and ts (time-add ts (seconds-to-time duration))))
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

(defun ip-forgejo--format-comments (comments)
  "Format a list of COMMENTS into Org-mode subheadings."
  (if (null comments)
      ""
    (let ((formatted-comments
           (mapcar
            (lambda (comment)
              (let* ((author (alist-get 'user comment))
                     (author-name (if author (alist-get 'login author) "Unknown"))
                     (created-str (alist-get 'created_at comment))
                     (body (ip-forgejo--clean-body (alist-get 'body comment)))
                     (timestamp (if created-str
                                    (condition-case nil
                                        (format-time-string "[%Y-%m-%d %a %H:%M]"
                                                            (date-to-time created-str))
                                      (error created-str))
                                  "")))
                (format "** Comment by %s %s\n%s" author-name timestamp (or body ""))))
            (ip-forgejo--ensure-list comments))))
      (string-join formatted-comments "\n"))))

(defun ip-forgejo--format-entry (issue times comments)
  "Format a single ISSUE with TIMES, COMMENTS, and metadata into an Org heading."
  (let* ((title (alist-get 'title issue))
         (number (alist-get 'number issue))
         (state (alist-get 'state issue))
         (todo (ip-forgejo--org-todo-state state))
         (labels (ip-forgejo--ensure-list (alist-get 'labels issue)))
         (owner-data (alist-get 'owner (alist-get 'repository issue)))
         (owner-name (if (stringp owner-data) owner-data (alist-get 'login owner-data)))
         (repo-name (alist-get 'name (alist-get 'repository issue)))
         (milestone (let ((m (alist-get 'milestone issue)))
                      (if m (alist-get 'title m) "")))
         (id (alist-get 'id issue))
         (body (ip-forgejo--clean-body (or (alist-get 'body issue) "")))
         (total-time (cl-reduce #'+ (mapcar (lambda (e) (alist-get 'time e 0))
                                            (ip-forgejo--ensure-list times))
                                :initial-value 0))
         (logbook (ip-forgejo--format-logbook times))
         (formatted-comments (ip-forgejo--format-comments comments))
         (org-id (org-id-new))
         ;; Generate Forgejo issue URL
         (base-url (car (ip-forgejo--current-config)))
         (web-url (replace-regexp-in-string "/api/v1$" "" base-url))
         (issue-url (format "%s/%s/%s/issues/%s" web-url owner-name repo-name number))
         ;; Extract dates
         (created (alist-get 'created_at issue))
         (due-date (alist-get 'deadline issue))
         ;; Format tags
         (label-tags (mapcar (lambda (lbl) (replace-regexp-in-string "[^A-Za-z0-9_]+" "_" (alist-get 'name lbl)))
                             labels))
         (all-tags (seq-uniq (append label-tags (list owner-name repo-name))))
         (tags-str (if all-tags (concat "    :" (mapconcat 'identity all-tags ":") ":") ""))
         ;; Format SCHEDULED and DEADLINE
         (scheduled-str (when created (format "SCHEDULED: <%s>" (ip-forgejo--format-org-timestamp created))))
         (deadline-str (when due-date (format "DEADLINE: <%s>" (ip-forgejo--format-org-timestamp due-date))))
         ;; Build the entry
         (properties-str (format ":PROPERTIES:\n:ID: %s\n:FORGEJO_ID: %s\n:FORGEJO_NUM: %s\n:FORGEJO_URL: %s\n:STATE: %s\n:CLIENT: %s\n:REPO: %s\n:MILESTONE: %s\n:TIME: %d\n:END:"
                                 org-id id number issue-url state owner-name repo-name milestone total-time))
         (logbook-block (if (string-empty-p logbook)
                            ""
                          (format ":LOGBOOK:\n%s\n:END:" logbook)))
         ;; Combine body and comments
         (main-content (string-join
                        (seq-filter #'identity
                                    (list body
                                          (unless (string-empty-p formatted-comments) formatted-comments)))
                        "\n\n")))
    (format "* %s %s%s\n%s\n%s\n%s\n%s\n\n%s"
            todo title tags-str
            (or scheduled-str "")
            (or deadline-str "")
            properties-str
            logbook-block
            main-content)))

(defun ip-forgejo--replace-or-insert-entry (forgejo-id entry)
  "Find Org heading by FORGEJO_ID and replace its subtree with ENTRY."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format ":FORGEJO_ID:[ \t]+%s" forgejo-id) nil t)
        (let ((beg (save-excursion
                     (goto-char (match-beginning 0))
                     (org-backward-heading-same-level 1)
                     (point))))
          (goto-char beg)
          (forward-line 1)
          (while (and (not (eobp)) (looking-at "^\\s-*\\(?:\\*+\\|:\\)"))
            (forward-line 1))
          (delete-region beg (point))
          (let ((inhibit-modification-hooks t))
            (insert entry)
            (insert "\n"))
          (ip-forgejo--log 'success "Updated existing entry: %s" forgejo-id))
      (goto-char (point-max))
      (let ((inhibit-modification-hooks t))
        (insert entry)
        (insert "\n\n"))
      (ip-forgejo--log 'success "Inserted new entry: %s" forgejo-id))))

;;;###autoload
(defun ip-forgejo--push-issue (forgejo-id title body state &optional repo-owner repo-name)
  "Update remote Forgejo issue with new state, body, and title."
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (token (cdr config))
         (owner (or repo-owner (org-entry-get nil "CLIENT")))
         (repo (or repo-name (org-entry-get nil "REPO")))
         (url (format "%s/repos/%s/%s/issues/%d" base-url owner repo forgejo-id))
         (patch-data `((title . ,title)
                       (body . ,body)
                       (state . ,state))))
    (unless (and owner repo)
      (ip-forgejo--log 'error "Cannot determine repository owner or name for issue %d" forgejo-id)
      (error "Cannot determine repository owner or name for issue %d" forgejo-id))
    (ip-forgejo--log 'info "Pushing update to issue %d" forgejo-id)
    (request url
      :type "PATCH"
      :headers `(("Authorization" . ,(concat "token " token))
                 ("Content-Type" . "application/json"))
      :data (json-serialize patch-data)
      :sync t
      :success (cl-function (lambda (&key &allow-other-keys)
                              (ip-forgejo--log 'success "Issue %d updated" forgejo-id)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (ip-forgejo--log 'error "Failed to update issue %d: %s"
                                             forgejo-id error-thrown))))))

;;;###autoload
(defun ip-forgejo--push-deadline (forgejo-id &optional repo-owner repo-name)
  "Push DEADLINE from current Org entry to Forgejo issue's duedate."
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (token (cdr config))
         (owner (or repo-owner (org-entry-get nil "CLIENT")))
         (repo (or repo-name (org-entry-get nil "REPO")))
         (url (format "%s/repos/%s/%s/issues/%d" base-url owner repo forgejo-id))
         (deadline-str (org-entry-get nil "DEADLINE"))
         (deadline-iso8601 (when deadline-str
                             (format-time-string "%FT%T%z"
                                                 (org-time-string-to-time deadline-str)))))
    (unless (and owner repo)
      (ip-forgejo--log 'error "Cannot determine repository owner or name for issue %d" forgejo-id)
      (error "Cannot determine repository owner or name for issue %d" forgejo-id))
    (when deadline-iso8601
      (let ((patch-data `((deadline . ,deadline-iso8601))))
        (ip-forgejo--log 'info "Pushing deadline to issue %d: %s" forgejo-id deadline-iso8601)
        (request url
          :type "PATCH"
          :headers `(("Authorization" . ,(concat "token " token))
                     ("Content-Type" . "application/json"))
          :data (json-serialize patch-data)
          :sync t
          :success (cl-function (lambda (&key &allow-other-keys)
                                  (ip-forgejo--log 'success "Deadline for issue %d updated" forgejo-id)))
          :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                                (ip-forgejo--log 'error "Failed to update deadline for issue %d: %s"
                                                 forgejo-id error-thrown))))))))

;;;###autoload
(defun ip-forgejo--add-time-entry (issue-id time-seconds &optional repo-owner repo-name)
  "Log time entry to Forgejo issue."
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (token (cdr config))
         (owner (or repo-owner (org-entry-get nil "CLIENT")))
         (repo (or repo-name (org-entry-get nil "REPO")))
         (url (format "%s/repos/%s/%s/issues/%d/times" base-url owner repo issue-id))
         (post-data `((created . ,(format-time-string "%FT%T%z"))
                     (time . ,time-seconds))))
    (unless (and owner repo)
      (ip-forgejo--log 'error "Cannot determine repository owner or name for issue %d" issue-id)
      (error "Cannot determine repository owner or name for issue %d" issue-id))
    (request url
      :type "POST"
      :headers `(("Authorization" . ,(concat "token " token))
                 ("Content-Type" . "application/json"))
      :data (json-serialize post-data)
      :sync t
      :success (cl-function (lambda (&key &allow-other-keys)
                              (ip-forgejo--log 'success "Time logged for issue %d" issue-id)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (ip-forgejo--log 'error "Failed to log time for issue %d: %s"
                                             issue-id error-thrown))))))

;;;###autoload
(defun ip-forgejo-list-clients ()
  "Show a list of all clients (owners) from imported issues."
  (interactive)
  (ip-debug-log 'info 'forgejo "Listing clients from imported issues")
  (let (clients)
    (org-map-entries
     (lambda ()
       (let ((client (org-entry-get nil "CLIENT")))
         (when client
           (cl-pushnew client clients :test 'equal))))
     "FORGEJO_ID>\"\"") ; only imported issues
    (with-current-buffer (get-buffer-create "*Forgejo Clients*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Clients (Org Owners) ===\n\n")
        (dolist (client (seq-sort 'string< clients))
          (insert (format "- %s\n" client)))
        (goto-char (point-min))
        (display-buffer (current-buffer))))
    (ip-debug-log 'success 'forgejo "Found %d unique clients" (length clients))))

;;;###autoload
(defun ip-forgejo-import-my-issues ()
  "Import or update assigned Forgejo issues in the current Org buffer."
  (interactive)
  (let* ((config (ip-forgejo--current-config))
         (base-url (car config))
         (search-url (concat base-url "/repos/issues/search"
                             "?state=all&type=issues"
                             "&assigned=true&created=false"
                             "&mentioned=false&review_requested=false"
                             "&reviewed=false&limit=100"))
         issues)
    (let ((org-element-use-cache nil)
          (font-lock-mode nil)
          (inhibit-modification-hooks t)
          (gc-cons-threshold (* 100000000 1)))
      ;; Initialize sync report buffer
      (with-current-buffer (get-buffer-create "*Forgejo Sync Report*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (ip-forgejo--log 'info "=== Forgejo Synchronization Report ===")
          (ip-forgejo--log 'info "Time: %s" (current-time-string))
          (ip-forgejo--log 'info "Instance: %s" ip-forgejo-current-instance)
          (ip-forgejo--log 'info "Base URL: %s" base-url)
          (ip-forgejo--log 'info "Search URL: %s" search-url)
          (condition-case err
              (progn
                (setq issues (ip-forgejo--api search-url))
                (if issues
                    (progn
                      (ip-forgejo--log 'success "API Request: Success")
                      (ip-forgejo--log 'info "Total issues retrieved: %d" (length issues))
                      (ip-forgejo--log 'info "Issues processed:")
                      (dolist (issue issues)
                        (let* ((title (alist-get 'title issue))
                               (state (alist-get 'state issue))
                               (repo (alist-get 'repository issue))
                               (repo-name (alist-get 'name repo))
                               (number (alist-get 'number issue)))
                          (ip-forgejo--log 'info "  • [%s/%d] %s [%s]" repo-name number title state))))
                    (ip-forgejo--log 'warning "API Request: Success, but empty list.")))
            (error
             (ip-forgejo--log 'error "API Request: FAILED — %S" err)
             (setq issues nil)))
          (ip-forgejo--log 'info "=== Raw Response (first issue) ===")
          (if issues
              (let ((str (prin1-to-string (car issues))))
                (ip-forgejo--log 'info "%s" (if (> (length str) 1000)
                                                (concat (substring str 0 1000) " [...]")
                                              str)))
            (ip-forgejo--log 'info "nil (no issues)"))
          (ip-forgejo--log 'info "=== Processing Issues ===")
          (goto-char (point-min))
          (display-buffer (current-buffer))))
      (unless issues
        (ip-debug-log 'error 'forgejo "No issues received from API")
        (user-error "No issues received from API"))
      (save-excursion
        (dolist (issue issues)
          (let* ((repo (alist-get 'repository issue))
                 (owner-data (alist-get 'owner repo))
                 (owner-name (if (stringp owner-data)
                                 owner-data
                               (alist-get 'login owner-data)))
                 (repo-name (alist-get 'name repo))
                 (index (alist-get 'number issue))
                 (times-url (format "%s/repos/%s/%s/issues/%s/times"
                                    base-url
                                    owner-name
                                    repo-name
                                    index))
                 (comments-url (format "%s/repos/%s/%s/issues/%s/comments"
                                       base-url
                                       owner-name
                                       repo-name
                                       index))
                 times comments entry title)
            ;; Get time logs
            (condition-case err
                (progn
                  (setq times (ip-forgejo--api times-url))
                  (setq times (ip-forgejo--ensure-list times)))
              (error
               (ip-forgejo--log 'warning "Failed to fetch times for #%d: %s" index (error-message-string err))
               (setq times nil)))
            ;; Get comments
            (condition-case err
                (progn
                  (setq comments (ip-forgejo--api comments-url))
                  (setq comments (ip-forgejo--ensure-list comments)))
              (error
               (ip-forgejo--log 'warning "Failed to fetch comments for #%d: %s" index (error-message-string err))
               (setq comments nil)))
            (setq title (alist-get 'title issue))
            (setq entry (ip-forgejo--format-entry issue times comments))
            (ip-forgejo--replace-or-insert-entry (alist-get 'id issue) entry)
            (ip-forgejo--log 'success "✓ Imported: %s" title)))
        (ip-forgejo--log 'success "=== Import Complete ===")
        (ip-debug-log 'success 'forgejo "Import completed: %d issues processed" (length issues))))))

;;;###autoload
(defun ip-forgejo--on-save-hook ()
  "Push deadline and state if current entry is a Forgejo issue."
  (when (org-before-first-heading-p)
    (cl-return-from ip-forgejo--on-save-hook))
  (save-excursion
    (org-back-to-heading t)
    (let ((forgejo-id (org-entry-get nil "FORGEJO_ID")))
      (when forgejo-id
        (ip-debug-log 'info 'forgejo "Auto-syncing deadline for issue %s" forgejo-id)
        (ip-forgejo--push-deadline (string-to-number forgejo-id))))))

;;;###autoload
(defun ip-forgejo-show-sync-report ()
  "Display the Forgejo sync report buffer."
  (interactive)
  (let ((buffer (get-buffer "*Forgejo Sync Report*")))
    (if buffer
        (display-buffer buffer)
      (message "No sync report available. Run ip-forgejo-import-my-issues first."))))

(add-hook 'org-after-save-hook 'ip-forgejo--on-save-hook)

(provide 'ip-forgejo)
;;; ip-forgejo.el ends here