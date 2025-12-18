;;; ip-tax.el --- Tax tracking and Beancount integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 IP Management System
;; Author: IP Management System
;; Version: 2.5
;; Keywords: org, tax, accounting, beancount
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Commentary:

;; Tax tracking and ledger management module for IP management system.
;; Provides Beancount integration for freelance income and expense tracking.
;;
;; Features:
;; - Interactive setup wizard for tax accounts
;; - Manual income/expense entry with smart defaults
;; - Tax payment recording with jurisdiction and type selection
;; - Deadline checking and warnings
;; - Beancount ledger file management
;; - Payment slip generation (with QR for supported jurisdictions)
;; - Flexible tax configuration via org file
;;
;; Usage:
;;   M-x ip-tax-setup              ; Interactive setup wizard
;;   M-x ip-tax-record-income      ; Record income transaction
;;   M-x ip-tax-record-expense     ; Record expense transaction
;;   M-x ip-tax-record-tax-payment ; Record tax payment
;;   M-x ip-tax-show-ledger        ; View ledger file
;;   M-x ip-tax-generate-payment   ; Generate payment slip

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'calendar)
(require 'ip-core)

;; External dependencies
(declare-function ip-debug-log "ip-debug" (level module message &rest args))

;; Optional Beancount support
(condition-case nil
    (require 'beancount)
  (error
   (defun ip-tax--beancount-mode-available-p ()
     "Check if beancount-mode is available."
     nil)))

;; Fallback logging if ip-debug is not available
(condition-case nil
    (require 'ip-debug)
  (error
   (defun ip-debug-log (level module message &rest args)
     "Fallback logging function."
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
     "Fallback debug macro."
     `(ip-debug-log 'info ,module ,message ,@args))))

;;; Customization

(defgroup ip-tax nil
  "Tax and ledger tracking for IP management system."
  :group 'ip-core
  :prefix "ip-tax-")

(defcustom ip-tax-enabled nil
  "Enable automatic tax and ledger tracking.
When non-nil, transactions are recorded to the Beancount ledger file."
  :type 'boolean
  :group 'ip-tax)

(defcustom ip-tax-ledger-file
  (expand-file-name "tax.beancount" ip-org-directory)
  "Path to Beancount ledger file for tax tracking."
  :type 'file
  :group 'ip-tax)

(defcustom ip-tax-config-file
  (expand-file-name "tax.org" ip-org-directory)
  "Path to tax configuration file."
  :type 'file
  :group 'ip-tax)

(defcustom ip-tax-default-currency "EUR"
  "Default currency for transactions."
  :type 'string
  :group 'ip-tax)

(defcustom ip-tax-deadline-warning-days 5
  "Days before tax deadline to show warning."
  :type 'integer
  :group 'ip-tax)

(defcustom ip-tax-enable-qr-generation t
  "Enable QR code generation for payments where supported."
  :type 'boolean
  :group 'ip-tax)

(defcustom ip-tax-payment-slip-directory
  (expand-file-name "payments" ip-org-directory)
  "Directory to save payment slips."
  :type 'directory
  :group 'ip-tax)

(defcustom ip-tax-serbian-ips-enabled t
  "Enable IPS (Instant Payment System) for Serbian payments."
  :type 'boolean
  :group 'ip-tax)

;;; Internal State

(defvar ip-tax--accounts-cache nil
  "Cache for account structure.")

(defvar ip-tax--jurisdiction-cache nil
  "Cache for jurisdiction configuration.")

(defvar ip-tax--payment-counter (make-hash-table :test 'equal)
  "Counter for generating unique payment references per jurisdiction.")

;;; Tax Configuration Management

(defun ip-tax--ensure-config ()
  "Create tax configuration file if it doesn't exist."
  (let ((config-path (expand-file-name ip-tax-config-file)))
    (unless (file-exists-p config-path)
      (make-directory (file-name-directory config-path) t)
      (with-temp-file config-path
        (insert "#+TITLE: Tax Configuration\n")
        (insert "#+AUTHOR: IP Management System\n")
        (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
        
        (insert "* Serbia (RS)\n")
        (insert "  :PROPERTIES:\n")
        (insert "  :CUSTOM_ID: serbia-rs\n")
        (insert "  :CURRENCY: RSD\n")
        (insert "  :TAX_ID_TYPE: JMBG\n")
        (insert "  :TAX_ID: 0101990800019\n")
        (insert "  :COMPANY_NAME: STEPAN ILLICHEVSKII PR BEOGRAD\n")
        (insert "  :ADDRESS: Београд-Звездара, Београд (Звездара), УСТАНИЧКА бр. 175, стан 52, спрат 5\n")
        (insert "  :BANK_ACCOUNT: 000-0000000-00\n")
        (insert "  :SUPPORTS_IPS: true\n")
        (insert "  :QR_STANDARD: IPS\n")
        (insert "  :END:\n\n")
        
        (insert "** Taxes\n")
        
        (insert "*** pio\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: Penziono osiguranje\n")
        (insert "    :ACCOUNT: 160-1620100000633-66\n")
        (insert "    :MODEL: 91\n")
        (insert "    :SHIFR: 402\n")
        (insert "    :PAYMENT_CODE: 7212\n")
        (insert "    :DEADLINE: 15th of next month\n")
        (insert "    :PERIOD: monthly\n")
        (insert "    :DESCRIPTION: Penziono osiguranje za {month} {year}\n")
        (insert "    :RECIPIENT: RFZO\n")
        (insert "    :END:\n\n")
        
        (insert "*** zdro\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: Zdravstveno osiguranje\n")
        (insert "    :ACCOUNT: 160-1620100000633-66\n")
        (insert "    :MODEL: 91\n")
        (insert "    :SHIFR: 401\n")
        (insert "    :PAYMENT_CODE: 7211\n")
        (insert "    :DEADLINE: 15th of next month\n")
        (insert "    :PERIOD: monthly\n")
        (insert "    :DESCRIPTION: Zdravstveno osiguranje za {month} {year}\n")
        (insert "    :RECIPIENT: RFZO\n")
        (insert "    :END:\n\n")
        
        (insert "*** nez\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: Osiguranje od nezaposlenosti\n")
        (insert "    :ACCOUNT: 160-1620100000633-66\n")
        (insert "    :MODEL: 91\n")
        (insert "    :SHIFR: 403\n")
        (insert "    :PAYMENT_CODE: 7213\n")
        (insert "    :DEADLINE: 15th of next month\n")
        (insert "    :PERIOD: monthly\n")
        (insert "    :DESCRIPTION: Osiguranje od nezaposlenosti za {month} {year}\n")
        (insert "    :RECIPIENT: RFZO\n")
        (insert "    :END:\n\n")
        
        (insert "*** income\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: Porez na prihod\n")
        (insert "    :ACCOUNT: 840-721313843-74\n")
        (insert "    :MODEL: 97\n")
        (insert "    :SHIFR: 253\n")
        (insert "    :PAYMENT_CODE: 7214\n")
        (insert "    :DEADLINE: 15th of next month\n")
        (insert "    :PERIOD: monthly\n")
        (insert "    :DESCRIPTION: Porez na paušalni prihod za {month} {year}\n")
        (insert "    :RECIPIENT: Poreska uprava Republike Srbije\n")
        (insert "    :END:\n\n")
        
        (insert "*** vat\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: PDV\n")
        (insert "    :ACCOUNT: 840-1684666-92\n")
        (insert "    :MODEL: 97\n")
        (insert "    :SHIFR: 121\n")
        (insert "    :PAYMENT_CODE: 7215\n")
        (insert "    :DEADLINE: 20th of next month\n")
        (insert "    :PERIOD: quarterly\n")
        (insert "    :DESCRIPTION: PDV za {quarter}. kvartal {year}\n")
        (insert "    :RECIPIENT: Poreska uprava Republike Srbije\n")
        (insert "    :END:\n\n")
        
        (insert "* Germany (DE)\n")
        (insert "  :PROPERTIES:\n")
        (insert "  :CUSTOM_ID: germany-de\n")
        (insert "  :CURRENCY: EUR\n")
        (insert "  :TAX_ID_TYPE: Steuernummer\n")
        (insert "  :SUPPORTS_IPS: false\n")
        (insert "  :QR_STANDARD: SEPA\n")
        (insert "  :END:\n\n")
        
        (insert "** Taxes\n")
        
        (insert "*** income_tax\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: Income Tax\n")
        (insert "    :IBAN: DE89370400440532013000\n")
        (insert "    :BIC: COBADEFFXXX\n")
        (insert "    :DEADLINE: 10th of next month\n")
        (insert "    :PERIOD: monthly\n")
        (insert "    :DESCRIPTION: Income tax for {month} {year}\n")
        (insert "    :RECIPIENT: Bundeszentralamt für Steuern\n")
        (insert "    :END:\n\n")
        
        (insert "*** vat\n")
        (insert "    :PROPERTIES:\n")
        (insert "    :NAME: VAT\n")
        (insert "    :IBAN: DE89370400440532013000\n")
        (insert "    :BIC: COBADEFFXXX\n")
        (insert "    :DEADLINE: 10th of next month\n")
        (insert "    :PERIOD: monthly\n")
        (insert "    :DESCRIPTION: VAT for {month} {year}\n")
        (insert "    :RECIPIENT: Bundeszentralamt für Steuern\n")
        (insert "    :END:\n\n")
        
        (insert "* Payment Templates\n")
        (insert "** Serbian Payment\n")
        (insert "   Recipient: {recipient}\n")
        (insert "   Account: {account}\n")
        (insert "   Amount: {amount} {currency}\n")
        (insert "   Reference: {reference}\n")
        (insert "   Purpose: {description}\n")
        (insert "   Payment Code: {payment_code}\n")
        (insert "   Shifr: {shifr}\n\n")
        
        (insert "** SEPA Payment\n")
        (insert "   Recipient: {recipient}\n")
        (insert "   IBAN: {iban}\n")
        (insert "   BIC: {bic}\n")
        (insert "   Amount: {amount} {currency}\n")
        (insert "   Reference: {reference}\n")
        (insert "   Purpose: {description}\n"))
      
      (ip-debug-log 'success 'tax "Created tax config: %s" config-path))
    config-path))

(defun ip-tax--parse-config (&optional refresh)
  "Parse tax configuration file and cache results."
  (ip-debug-log 'info 'tax "Start parsing config (refresh=%s)" refresh)

  (when (or refresh (null ip-tax--jurisdiction-cache))
    (ip-tax--ensure-config)

    (with-current-buffer (find-file-noselect ip-tax-config-file)
      (org-with-wide-buffer
        (setq ip-tax--jurisdiction-cache nil)

        (let ((jurisdictions-alist nil))  ;; Ассоциативный список: position -> jurisdiction

          ;; -------- Сначала собираем все юрисдикции и их позиции
          (org-map-entries
           (lambda ()
             (let* ((title (nth 4 (org-heading-components)))
                    (pos (point))
                    (props (org-entry-properties))
                    code jurisdiction)

               (ip-debug-log 'debug 'tax
                             "Level 1 heading: %S at pos %d"
                             title pos)

               ;; отрезаем Payment Templates
               (when (string-match "(\\([A-Z][A-Z]\\))$" title)
                 (setq code (match-string 1 title))
                 (setq jurisdiction
                       (list
                        :name title
                        :code code
                        :currency (or (cdr (assoc "CURRENCY" props))
                                      ip-tax-default-currency)
                        :tax-id-type (cdr (assoc "TAX_ID_TYPE" props))
                        :tax-id (cdr (assoc "TAX_ID" props))
                        :company-name (cdr (assoc "COMPANY_NAME" props))
                        :address (cdr (assoc "ADDRESS" props))
                        :bank-account (cdr (assoc "BANK_ACCOUNT" props))
                        :supports-ips
                        (equal (cdr (assoc "SUPPORTS_IPS" props)) "true")
                        :qr-standard (cdr (assoc "QR_STANDARD" props))
                        :taxes nil))

                 (ip-debug-log 'debug 'tax
                               "Created jurisdiction: %s (%s)"
                               title code)

                 ;; Сохраняем jurisdiction с её позицией
                 (push (cons pos jurisdiction) jurisdictions-alist))))
           "LEVEL=1"
           'file)

          ;; -------- Теперь обрабатываем налоги и распределяем по юрисдикциям
          (org-map-entries
           (lambda ()
             (let* ((title (nth 4 (org-heading-components)))
                    (pos (point))
                    (props (org-entry-properties))
                    (parent-pos (save-excursion
                                  (org-up-heading-safe)
                                  (point)))
                    jurisdiction)

               (ip-debug-log 'debug 'tax
                             "Level 3 heading: %S at pos %d, parent at %d"
                             title pos parent-pos)

               ;; Находим jurisdiction, которой принадлежит этот налог
               ;; Ищем ближайшую юрисдикцию выше текущей позиции
               (setq jurisdiction nil)
               (dolist (entry jurisdictions-alist)
                 (when (and (< (car entry) pos)
                            (or (null jurisdiction)
                                (> (car entry) (car (assoc parent-pos (list (cons parent-pos jurisdiction)))))))
                   (setq jurisdiction (cdr entry))))

               (when jurisdiction
                 (let ((tax
                        (list
                         :type title
                         :name (cdr (assoc "NAME" props))
                         :account (cdr (assoc "ACCOUNT" props))
                         :iban (cdr (assoc "IBAN" props))
                         :bic (cdr (assoc "BIC" props))
                         :model (cdr (assoc "MODEL" props))
                         :shifr (cdr (assoc "SHIFR" props))
                         :payment-code (cdr (assoc "PAYMENT_CODE" props))
                         :deadline-spec (cdr (assoc "DEADLINE" props))
                         :period (cdr (assoc "PERIOD" props))
                         :description (cdr (assoc "DESCRIPTION" props))
                         :recipient (cdr (assoc "RECIPIENT" props)))))

                   (ip-debug-log 'debug 'tax
                                 "Adding tax %s to %s"
                                 title
                                 (plist-get jurisdiction :name))

                   (plist-put jurisdiction
                              :taxes
                              (append (plist-get jurisdiction :taxes)
                                      (list tax)))))))
           "LEVEL=3"
           'file)

          ;; Преобразуем alist в список jurisdictions для кэша
          (setq ip-tax--jurisdiction-cache
                (mapcar #'cdr jurisdictions-alist)))))

    (setq ip-tax--jurisdiction-cache
          (nreverse ip-tax--jurisdiction-cache))

    (ip-debug-log 'info 'tax
                  "Parsed %d jurisdictions"
                  (length ip-tax--jurisdiction-cache)))

  ip-tax--jurisdiction-cache)

(defun ip-tax--get-jurisdictions (&optional refresh)
  "Get list of configured jurisdictions."
  (let ((config (ip-tax--parse-config refresh)))
      (ip-debug-log 'debug 'tax
                  "Jurisdictions config loaded: %d entries"
                  (length config))
    (when config
      (mapcar (lambda (j) (cons (plist-get j :name) (plist-get j :code)))
              config))))

(defun ip-tax--get-jurisdiction-by-code (code &optional refresh)
  "Get jurisdiction configuration by CODE."
  (let ((config (ip-tax--parse-config refresh)))
    (when config
      (cl-find-if (lambda (j) (equal (plist-get j :code) code))
                  config))))

(defun ip-tax--get-tax-types (jurisdiction-code)
  "Get tax types for JURISDICTION-CODE."
  (let ((jurisdiction (ip-tax--get-jurisdiction-by-code jurisdiction-code)))
    (when (and jurisdiction (plist-get jurisdiction :taxes))
      (mapcar (lambda (tax-item)  ; Изменил имя переменной
                (cons (plist-get tax-item :name) (plist-get tax-item :type)))
              (plist-get jurisdiction :taxes)))))

(defun ip-tax--get-tax-config (jurisdiction-code tax-type)
  "Get tax configuration for JURISDICTION-CODE and TAX-TYPE."
  (let ((jurisdiction (ip-tax--get-jurisdiction-by-code jurisdiction-code)))
    (when (and jurisdiction (plist-get jurisdiction :taxes))
      (cl-find-if (lambda (tax-item)  ; Изменил имя переменной
                    (equal (plist-get tax-item :type) tax-type))
                  (plist-get jurisdiction :taxes)))))

;;; Date and Deadline Utilities

(defun ip-tax--parse-deadline-spec (spec &optional reference-month reference-year)
  "Parse deadline specification like '15th of next month'.
REFERENCE-MONTH and REFERENCE-YEAR define the base month (default current)."
  (let* ((today (decode-time))
         (ref-month (or reference-month (nth 4 today)))
         (ref-year (or reference-year (nth 5 today))))
    
    (cond
     ((string-match "^\\([0-9]+\\)th of next month$" spec)
      (let* ((day (string-to-number (match-string 1 spec)))
             (next-month (if (= ref-month 12) 1 (1+ ref-month)))
             (next-year (if (= ref-month 12) (1+ ref-year) ref-year)))
        (list day next-month next-year)))
     
     ((string-match "^\\([0-9]+\\)th of month$" spec)
      (let ((day (string-to-number (match-string 1 spec))))
        (list day ref-month ref-year)))
     
     ((string-match "^\\([0-9]+\\) \\([a-zA-Z]+\\)$" spec)
      (let* ((day (string-to-number (match-string 1 spec)))
             (month-name (match-string 2 spec))
             (month (ip-tax--month-name-to-number month-name)))
        (list day month (if (< month ref-month) (1+ ref-year) ref-year))))
     
     (t
      (error "Unknown deadline specification: %s" spec)))))

(defun ip-tax--month-name-to-number (name)
  "Convert month NAME to number (1-12)."
  (let ((months '(("january" . 1) ("february" . 2) ("march" . 3)
                  ("april" . 4) ("may" . 5) ("june" . 6)
                  ("july" . 7) ("august" . 8) ("september" . 9)
                  ("october" . 10) ("november" . 11) ("december" . 12)
                  ("januar" . 1) ("februar" . 2) ("mart" . 3)
                  ("april" . 4) ("maj" . 5) ("jun" . 6)
                  ("jul" . 7) ("avgust" . 8) ("septembar" . 9)
                  ("oktobar" . 10) ("novembar" . 11) ("decembar" . 12))))
    (cdr (assoc (downcase name) months))))

(defun ip-tax--is-deadline-passed (deadline-spec &optional payment-month payment-year)
  "Check if deadline has passed for given payment month/year."
  (let* ((pay-month (or payment-month (nth 4 (decode-time))))
         (pay-year (or payment-year (nth 5 (decode-time))))
         (deadline-date (ip-tax--parse-deadline-spec deadline-spec pay-month pay-year))
         (deadline-time (encode-time 0 0 0
                                    (nth 0 deadline-date)
                                    (nth 1 deadline-date)
                                    (nth 2 deadline-date)))
         (now (current-time)))
    (time-less-p deadline-time now)))

(defun ip-tax--days-until-deadline (deadline-spec &optional payment-month payment-year)
  "Calculate days until deadline."
  (let* ((pay-month (or payment-month (nth 4 (decode-time))))
         (pay-year (or payment-year (nth 5 (decode-time))))
         (deadline-date (ip-tax--parse-deadline-spec deadline-spec pay-month pay-year))
         (deadline-time (encode-time 0 0 0
                                    (nth 0 deadline-date)
                                    (nth 1 deadline-date)
                                    (nth 2 deadline-date)))
         (now (current-time))
         (seconds-left (- (float-time deadline-time) (float-time now))))
    (if (< seconds-left 0)
        (- (floor (/ seconds-left 86400)))  ; Negative = days overdue
      (floor (/ seconds-left 86400)))))     ; Positive = days left

;;; Payment Reference Generation

(defun ip-tax--generate-payment-reference (jurisdiction-code tax-type)
  "Generate unique payment reference for tax payment."
  (let* ((key (format "%s-%s" jurisdiction-code tax-type))
         (counter (or (gethash key ip-tax--payment-counter) 0))
         (today (format-time-string "%y%m%d"))
         (new-counter (1+ counter))
         (reference (format "%s-%06d" today new-counter)))
    
    (puthash key new-counter ip-tax--payment-counter)
    reference))

(defun ip-tax--generate-serbian-reference (tax-config)
  "Generate Serbian-style payment reference with model and control digits."
  (let* ((model (or (plist-get tax-config :model) "97"))
         (prefix (or (ip-tax--get-company-property :PAYMENT_PREFIX) "240229"))
         (tax-id (or (ip-tax--get-company-property :JMBG) "0000000000000"))
         (last4 (substring tax-id (- (length tax-id) 4)))
         (date (format-time-string "%y%m%d"))
         (counter (ip-tax--generate-payment-reference "RS" (plist-get tax-config :type)))
         (payment-num (concat prefix last4 date (substring counter 7)))
         (control (ip-tax--calculate-control-digits payment-num)))
    (format "%s-%s-%s" model payment-num control)))

;;; Beancount Integration

(defun ip-tax--ensure-ledger ()
  "Create ledger file if it doesn't exist."
  (let ((ledger-path (expand-file-name ip-tax-ledger-file)))
    (unless (file-exists-p ledger-path)
      (make-directory (file-name-directory ledger-path) t)
      (with-temp-file ledger-path
        (insert ";; Tax Ledger for IP Management System\n")
        (insert ";; Generated by ip-tax.el\n")
        (insert ";; File encoding: UTF-8\n\n")
        
        ;; Multiple operating currencies
        (insert "option \"operating_currency\" \"EUR\"\n")
        (insert "option \"operating_currency\" \"RSD\"\n")
        (insert "option \"title\" \"Tax Payments Ledger\"\n\n")
        
        ;; Account definitions based on configured jurisdictions and taxes
        (insert ";; Account definitions\n")
        (insert "2024-01-01 open Assets:Bank:Business\n")
        
        ;; Add expense accounts for configured taxes
        (let ((jurisdictions (ip-tax--get-jurisdictions t)))
          (dolist (jurisdiction-item jurisdictions)  ; Изменил имя переменной
            (let* ((code (cdr jurisdiction-item))
                   (jurisdiction-config (ip-tax--get-jurisdiction-by-code code))
                   (taxes-list (when jurisdiction-config (plist-get jurisdiction-config :taxes)))
                   (currency (when jurisdiction-config (plist-get jurisdiction-config :currency))))
              
              (when taxes-list
                (dolist (tax-item taxes-list)  ; Изменил имя переменной
                  (let ((tax-type (plist-get tax-item :type))
                        (tax-name (plist-get tax-item :name)))
                    (insert (format "2024-01-01 open Expenses:Taxes:%s:%s  %s  ; %s\n"
                                   code
                                   (upcase tax-type)
                                   currency
                                   tax-name))))))))
        
        (insert "\n"))
      
      (ip-debug-log 'success 'tax "Created ledger: %s" ledger-path))
    ledger-path))

(defun ip-tax--format-transaction (date description from-account to-account amount currency)
  "Format a Beancount transaction entry."
  (let ((formatted-amount (format "%.2f %s" 
                                  (if (stringp amount) (string-to-number amount) amount) 
                                  currency)))
    (format "%s * \"%s\"\n  %-40s  %s\n  %-40s -%s\n\n"
            date
            description
            to-account
            formatted-amount
            from-account
            formatted-amount)))

(defun ip-tax--record-to-ledger (date description from-account to-account amount currency)
  "Record a transaction in the Beancount ledger."
  (if (not ip-tax-enabled)
      (progn
        (ip-debug-log 'debug 'tax "Tax tracking disabled, skipping")
        nil)
    
    (ip-tax--ensure-ledger)
    
    (let ((entry (ip-tax--format-transaction date description from-account
                                             to-account amount currency)))
      (condition-case err
          (progn
            (with-temp-buffer
              (set-buffer-file-coding-system 'utf-8)
              (insert-file-contents ip-tax-ledger-file)
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert entry)
              (write-region (point-min) (point-max)
                            ip-tax-ledger-file nil 'silent))
            (ip-debug-log 'success 'tax
                          "Recorded: %s | %s | %.2f %s"
                          date description amount currency)
            t)
        (error
         (ip-debug-log 'error 'tax "Failed to record transaction: %s"
                       (error-message-string err))
         nil)))))

;;; Payment Slip Generation

(defun ip-tax--generate-payment-slip (payment-details)
  "Generate payment slip with details."
  (let* ((jurisdiction-code (plist-get payment-details :jurisdiction-code))
         (tax-type (plist-get payment-details :tax-type))
         (amount (plist-get payment-details :amount))
         (currency (plist-get payment-details :currency))
         (reference (plist-get payment-details :reference))
         (description (plist-get payment-details :description))
         (date (plist-get payment-details :date))
         (jurisdiction (ip-tax--get-jurisdiction-by-code jurisdiction-code))
         (tax-config (ip-tax--get-tax-config jurisdiction-code tax-type))
         (slip-filename (format "payment-%s-%s-%s.txt" 
                               jurisdiction-code tax-type 
                               (format-time-string "%Y%m%d-%H%M%S")))
         (slip-path (expand-file-name slip-filename ip-tax-payment-slip-directory)))
    
    (make-directory ip-tax-payment-slip-directory t)
    
    (with-temp-file slip-path
      (insert "========================================\n")
      (insert "PAYMENT SLIP\n")
      (insert "========================================\n\n")
      
      (insert (format "Date: %s\n" date))
      (insert (format "Jurisdiction: %s (%s)\n" 
                     (plist-get jurisdiction :name) jurisdiction-code))
      (insert (format "Tax Type: %s\n" (plist-get tax-config :name)))
      (insert (format "Amount: %.2f %s\n" amount currency))
      (insert (format "Reference: %s\n" reference))
      (insert (format "Description: %s\n\n" description))
      
      ;; Jurisdiction-specific details
      (cond
       ((equal jurisdiction-code "RS")
        (insert "--- Serbian Payment Details ---\n")
        (insert (format "Recipient: %s\n" (plist-get tax-config :recipient)))
        (insert (format "Account: %s\n" (plist-get tax-config :account)))
        (when (plist-get tax-config :model)
          (insert (format "Model: %s\n" (plist-get tax-config :model))))
        (when (plist-get tax-config :shifr)
          (insert (format "Shifr: %s\n" (plist-get tax-config :shifr))))
        (when (plist-get tax-config :payment-code)
          (insert (format "Payment Code: %s\n" (plist-get tax-config :payment-code))))
        
        (when (and ip-tax-serbian-ips-enabled 
                   (plist-get jurisdiction :supports-ips))
          (insert "\n--- IPS QR Code Data ---\n")
          (insert (ip-tax--generate-serbian-ips-qr-data payment-details))))
       
       ((equal jurisdiction-code "DE")
        (insert "--- SEPA Payment Details ---\n")
        (insert (format "Recipient: %s\n" (plist-get tax-config :recipient)))
        (insert (format "IBAN: %s\n" (plist-get tax-config :iban)))
        (when (plist-get tax-config :bic)
          (insert (format "BIC: %s\n" (plist-get tax-config :bic)))))
       
       (t
        (insert "--- Payment Details ---\n")
        (when (plist-get tax-config :recipient)
          (insert (format "Recipient: %s\n" (plist-get tax-config :recipient))))
        (when (plist-get tax-config :account)
          (insert (format "Account: %s\n" (plist-get tax-config :account))))
        (when (plist-get tax-config :iban)
          (insert (format "IBAN: %s\n" (plist-get tax-config :iban))))))
      
      (insert "\n========================================\n")
      (insert "Generated by IP Management System\n"))
    
    slip-path))

(defun ip-tax--generate-serbian-ips-qr-data (payment-details)
  "Generate IPS QR code data string for Serbian payments."
  (let* ((tax-config (ip-tax--get-tax-config 
                      (plist-get payment-details :jurisdiction-code)
                      (plist-get payment-details :tax-type)))
         (account (plist-get tax-config :account))
         (recipient (plist-get tax-config :recipient))
         (amount (plist-get payment-details :amount))
         (currency (plist-get payment-details :currency))
         ;;(reference (plist-get payment-details :reference))
         (description (plist-get payment-details :description))
         (payment-code (plist-get tax-config :payment-code)))
    
    ;; IPS QR format based on Serbian standard
    (format "IPS://%s?AM=%.2f&CC=%s&RN=%s&PT=%s&DT=%s"
            account
            amount
            currency
            (url-hexify-string recipient)
            payment-code
            (url-hexify-string description))))

;;; Main Interactive Functions

;;;###autoload
(defun ip-tax-setup ()
  "Interactive setup wizard for tax tracking."
  (interactive)
  (ip-debug-log 'info 'tax "Starting tax setup")
  
  ;; Create configuration file
  (ip-tax--ensure-config)
  
  ;; Create ledger file
  (ip-tax--ensure-ledger)
  
  ;; Create payments directory
  (make-directory ip-tax-payment-slip-directory t)
  
  ;; Show configuration
  (find-file ip-tax-config-file)
  (message "Tax setup complete! Configuration: %s, Ledger: %s"
           ip-tax-config-file ip-tax-ledger-file))

;;;###autoload
(defun ip-tax-record-income (&optional date description amount currency client-id)
  "Record income transaction to ledger."
  (interactive)
  (let* ((date (or date (read-string "Date (YYYY-MM-DD): " 
                                     (format-time-string "%Y-%m-%d"))))
         (desc (or description (read-string "Description: ")))
         (amt (or amount (read-number "Amount: ")))
         (curr (or currency (read-string "Currency: " ip-tax-default-currency)))
         (client (or client-id
                     (completing-read "Client (optional): "
                                      (cons "Other" (ip-list-client-ids))
                                      nil t)))
         (from-account (if (string= client "Other")
                           "Income:Freelance:Other"
                         (format "Income:Freelance:%s" client)))
         (to-account "Assets:Bank:Business"))
    
    (when (y-or-n-p (format "Record income: %s %.2f %s? " desc amt curr))
      (ip-tax--record-to-ledger date desc from-account to-account amt curr)
      (message "Income recorded: %s %.2f %s" desc amt curr))))

;;;###autoload
(defun ip-tax-record-expense (&optional date description amount currency category)
  "Record expense transaction to ledger."
  (interactive)
  (let* ((date (or date (read-string "Date (YYYY-MM-DD): " 
                                     (format-time-string "%Y-%m-%d"))))
         (desc (or description (read-string "Description: ")))
         (amt (or amount (read-number "Amount: ")))
         (curr (or currency (read-string "Currency: " ip-tax-default-currency)))
         (category (or category
                       (completing-read "Category: "
                                        '("Equipment" "Software" "Office" "Travel" 
                                          "Marketing" "Professional" "Other")
                                        nil t)))
         (from-account "Assets:Bank:Business")
         (to-account (format "Expenses:%s" category)))
    
    (when (y-or-n-p (format "Record expense: %s %.2f %s? " desc amt curr))
      (ip-tax--record-to-ledger date desc from-account to-account amt curr)
      (message "Expense recorded: %s %.2f %s" desc amt curr))))

;;;###autoload
(defun ip-tax-record-tax-payment ()
  "Record tax payment transaction with interactive selection."
  (interactive)
  
  ;; Refresh cache to ensure we have latest config
  (ip-tax--parse-config t)
  
  ;; Select jurisdiction
  (let* ((jurisdictions (ip-tax--get-jurisdictions))
         (jurisdiction-name (completing-read "Jurisdiction: " jurisdictions nil t))
         (jurisdiction-code (cdr (assoc jurisdiction-name jurisdictions)))
         (jurisdiction (ip-tax--get-jurisdiction-by-code jurisdiction-code))
         
         ;; Select tax type
         (tax-types (ip-tax--get-tax-types jurisdiction-code))
         (tax-type-name (completing-read "Tax type: " tax-types nil t))
         (tax-type (cdr (assoc tax-type-name tax-types)))
         (tax-config (ip-tax--get-tax-config jurisdiction-code tax-type))
         
         ;; Get payment details
         (date (read-string "Payment date (YYYY-MM-DD): " 
                            (format-time-string "%Y-%m-%d")))
         (amount (read-number (format "Amount (%s): " 
                                     (plist-get jurisdiction :currency))))
         (currency (plist-get jurisdiction :currency))
         (description (read-string "Description: " 
                                   (plist-get tax-config :description)))
         (month (string-to-number (substring date 5 7)))
         (year (string-to-number (substring date 0 4))))
    
    ;; Check if we have required config
    (unless jurisdictions
      (user-error "No jurisdictions configured. Run M-x ip-tax-setup first"))
    
    (unless tax-config
      (user-error "Tax configuration not found for %s in %s" tax-type jurisdiction-code))
    
    ;; Check deadline
    ; (let ((deadline-spec (plist-get tax-config :deadline-spec))
    ;       (deadline-days (when deadline-spec
    ;                        (ip-tax--days-until-deadline deadline-spec month year))))
      
    ;   (when (and deadline-days (< deadline-days 0))
    ;     (if (y-or-n-p (format "Warning: Payment is %d days overdue. Continue? " 
    ;                           (- deadline-days)))
    ;         (message "Recording overdue payment...")
    ;       (user-error "Payment cancelled")))
      
    ;   (when (and deadline-days (<= deadline-days ip-tax-deadline-warning-days))
    ;     (message "Note: Payment deadline in %d days" deadline-days)))
    
    ;; Generate reference
    (let* ((reference (if (equal jurisdiction-code "RS")
                          (ip-tax--generate-serbian-reference tax-config)
                        (ip-tax--generate-payment-reference jurisdiction-code tax-type)))
           (payment-details (list :jurisdiction-code jurisdiction-code
                                  :tax-type tax-type
                                  :amount amount
                                  :currency currency
                                  :reference reference
                                  :description description
                                  :date date
                                  :month month
                                  :year year))
           
           ;; Beancount accounts
           (from-account "Assets:Bank:Business")
           (to-account (format "Expenses:Taxes:%s:%s" jurisdiction-code (upcase tax-type))))
      
      ;; Record to ledger
      (when (y-or-n-p (format "Record tax payment: %s %.2f %s? " description amount currency))
        (ip-tax--record-to-ledger date description from-account to-account amount currency)
        
        ;; Generate payment slip
        (when (y-or-n-p "Generate payment slip? ")
          (let ((slip-path (ip-tax--generate-payment-slip payment-details)))
            (message "Payment slip generated: %s" slip-path)
            
            ;; Show QR code if enabled and supported
            (when (and ip-tax-enable-qr-generation
                       (plist-get jurisdiction :supports-ips)
                       (equal jurisdiction-code "RS"))
              (ip-tax--show-qr-code payment-details))))
        
        (message "Tax payment recorded: %s %.2f %s (Ref: %s)" 
                 description amount currency reference)))))

(defun ip-tax--show-qr-code (payment-details)
  "Display QR code for payment (if qrencode is available)."
  (let ((qr-data (ip-tax--generate-serbian-ips-qr-data payment-details))
        (qr-command (executable-find "qrencode")))
    (if qr-command
        (let ((temp-file (make-temp-file "payment-qr-" nil ".png")))
          (call-process qr-command nil nil nil 
                        "-o" temp-file
                        "-s" "10"
                        qr-data)
          (if (and (fboundp 'image-type-available-p)
                   (image-type-available-p 'png))
              (progn
                (switch-to-buffer (get-buffer-create "*Payment QR*"))
                (erase-buffer)
                (insert-image (create-image temp-file 'png nil))
                (display-buffer (current-buffer)))
            (message "QR code saved to: %s" temp-file)))
      (message "Install 'qrencode' to generate QR codes"))))

;;;###autoload
(defun ip-tax-show-ledger ()
  "Open the tax ledger file for viewing/editing."
  (interactive)
  (ip-tax--ensure-ledger)
  (find-file ip-tax-ledger-file)
  (when (and (featurep 'beancount)
             (fboundp 'beancount-mode))
    (beancount-mode)))

;;;###autoload
(defun ip-tax-show-config ()
  "Open the tax configuration file."
  (interactive)
  (ip-tax--ensure-config)
  (find-file ip-tax-config-file))

;;;###autoload
(defun ip-tax-list-payments ()
  "List generated payment slips."
  (interactive)
  (if (file-exists-p ip-tax-payment-slip-directory)
      (let ((files (directory-files ip-tax-payment-slip-directory t "\\.txt$")))
        (if files
            (with-current-buffer (get-buffer-create "*Tax Payments*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "=== Generated Payment Slips ===\n\n")
                (dolist (file files)
                  (insert (format "• %s\n" (file-name-nondirectory file))))
                (goto-char (point-min))
                (read-only-mode 1))
              (display-buffer (current-buffer)))
          (message "No payment slips found")))
    (message "Payments directory does not exist")))

;;;###autoload
(defun ip-tax-validate-ledger ()
  "Validate the Beancount ledger file."
  (interactive)
  (ip-tax--ensure-ledger)
  (let ((bean-check (executable-find "bean-check")))
    (if bean-check
        (let ((output (shell-command-to-string 
                       (format "%s %s" bean-check ip-tax-ledger-file))))
          (if (string-empty-p output)
              (message "Ledger validation successful")
            (message "Ledger validation errors:\n%s" output)))
      (message "bean-check not found. Install Beancount to validate."))))

;;; Helper Functions

(defun ip-tax--get-company-property (property &optional default)
  "Get PROPERTY from company.org, return DEFAULT if not found."
  (let ((company (ip-get-company-info)))
    (or (plist-get company property) default)))

(defun ip-tax--calculate-control-digits (number)
  "Calculate control digits for payment reference."
  (let ((sum 0))
    (dotimes (i (length number))
      (setq sum (+ sum (string-to-number (substring number i (1+ i))))))
    (format "%02d" (mod sum 97))))

(defun ip-tax--format-description (template date)
  "Format description from template using DATE."
  (let* ((desc template)
         (year (string-to-number (substring date 0 4)))
         (month (string-to-number (substring date 5 7)))
         (quarter (1+ (/ (1- month) 3)))
         (month-names '("januar" "februar" "mart" "april" "maj" "jun"
                       "jul" "avgust" "septembar" "oktobar" "novembar" "decembar")))
    (setq desc (replace-regexp-in-string "{year}" (number-to-string year) desc))
    (setq desc (replace-regexp-in-string "{month}" (nth (1- month) month-names) desc))
    (setq desc (replace-regexp-in-string "{quarter}" (number-to-string quarter) desc))
    desc))

;;; Minor Mode

(defvar ip-tax-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t s") 'ip-tax-setup)
    (define-key map (kbd "C-c t i") 'ip-tax-record-income)
    (define-key map (kbd "C-c t e") 'ip-tax-record-expense)
    (define-key map (kbd "C-c t p") 'ip-tax-record-tax-payment)
    (define-key map (kbd "C-c t l") 'ip-tax-show-ledger)
    (define-key map (kbd "C-c t c") 'ip-tax-show-config)
    (define-key map (kbd "C-c t v") 'ip-tax-validate-ledger)
    (define-key map (kbd "C-c t L") 'ip-tax-list-payments)
    map)
  "Keymap for `ip-tax-mode'.")

;;;###autoload
(define-minor-mode ip-tax-mode
  "Minor mode for tax tracking integration.

\\{ip-tax-mode-map}"
  :lighter " Tax"
  :keymap ip-tax-mode-map
  :group 'ip-tax
  (if ip-tax-mode
      (progn
        (ip-tax--ensure-config)
        (ip-tax--ensure-ledger)
        (ip-debug-log 'info 'tax "Tax mode enabled"))
    (ip-debug-log 'info 'tax "Tax mode disabled")))

(provide 'ip-tax)
;;; ip-tax.el ends here