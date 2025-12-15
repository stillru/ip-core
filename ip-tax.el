;;; ip-tax.el --- Tax tracking and Beancount integration for IP system -*- lexical-binding: t; -*-
;;; Commentary:
;; Независимый модуль учёта налогов и движения средств через Beancount.
;; Работает как сам по себе, так и в связке с ip-invoice.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ip-core)

;; Попытка загрузить beancount.el — если есть
(condition-case nil
    (require 'beancount)
  (error
   (defun ip-tax--warn-beancount ()
     "Warn if beancount.el is missing."
     (message "⚠️ ip-tax: Beancount.el not available — ledger will be plain text."))))

(defgroup ip-tax nil
  "Tax and ledger tracking for IP system."
  :group 'ip-core
  :prefix "ip-tax-")

(defcustom ip-tax-enabled nil
  "Enable tax/ledger tracking."
  :type 'boolean
  :group 'ip-tax)

(defcustom ip-tax-ledger-file "~/org/ip/tax.beancount"
  "Beancount ledger file for tax tracking."
  :type 'file
  :group 'ip-tax)

(defcustom ip-tax-income-account "Income:Freelance"
  "Default Beancount income account."
  :type 'string
  :group 'ip-tax)

(defcustom ip-tax-expense-account "Expenses:Tax:VAT"
  "Default Beancount tax expense account."
  :type 'string
  :group 'ip-tax)

(defcustom ip-tax-asset-account "Assets:Bank:Primary"
  "Beancount asset (bank) account."
  :type 'string
  :group 'ip-tax)

;; —————————————————————————————————————
;; Внутренние утилиты
;; —————————————————————————————————————

(defun ip-tax--ensure-ledger ()
  "Создаёт ledger-файл, если он не существует."
  (unless (file-exists-p ip-tax-ledger-file)
    (make-directory (file-name-directory ip-tax-ledger-file) t)
    (with-temp-file ip-tax-ledger-file
      (insert ";; Tax ledger for IP system — managed by ip-tax.el\n"))
    (ip-debug-log 'success 'tax "Created ledger: %s" ip-tax-ledger-file)))

(defun ip-tax--format-beancount-amount (amount currency)
  "Форматирует сумму в виде '100.00 EUR'."
  (format "%.2f %s" (if (stringp amount) (string-to-number amount) amount) currency))

(defun ip-tax--record-transaction (date description amount currency income-account)
  "Записывает транзакцию в ledger.
AMOUNT — положительный для прихода, отрицательный для расхода.
INCOME-ACCOUNT используется как контрсчёт (например, Income:Freelance или Expenses:Tax)."
  (unless ip-tax-enabled
    (ip-debug-log 'debug 'tax "Tax tracking is disabled — skipping transaction")
    (cl-return-from ip-tax--record-transaction nil))

  (ip-tax--ensure-ledger)

  (let* ((formatted-amount (ip-tax--format-beancount-amount (abs amount) currency))
         (asset-posting    (format "%-30s %s" ip-tax-asset-account
                                   (if (> amount 0) formatted-amount (concat "-" formatted-amount))))
         (counter-posting  (format "%-30s %s" income-account
                                   (if (> amount 0) (concat "-" formatted-amount) formatted-amount)))
         (entry (format "%s * \"%s\"\n  %s\n  %s\n\n"
                        date description asset-posting counter-posting)))

    (with-temp-file ip-tax-ledger-file
      (insert-file-contents ip-tax-ledger-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert entry))

    (ip-debug-log 'success 'tax "Recorded: %s | %s | %s" date description amount)))

;; —————————————————————————————————————
;; Публичный API
;; —————————————————————————————————————

;;;###autoload
(defun ip-tax-record-income ()
  "Интерактивно записать приход."
  (interactive)
  (let ((date (read-string "Date (YYYY-MM-DD): " (format-time-string "%Y-%m-%d")))
        (desc (read-string "Description: "))
        (amount (read-number "Amount (positive): "))
        (curr (read-string "Currency: " "EUR")))
    (ip-tax--record-transaction date desc amount curr ip-tax-income-account)))

;;;###autoload
(defun ip-tax-record-expense ()
  "Интерактивно записать расход (например, налог)."
  (interactive)
  (let ((date (read-string "Date (YYYY-MM-DD): " (format-time-string "%Y-%m-%d")))
        (desc (read-string "Description: "))
        (amount (read-number "Amount (positive): "))
        (curr (read-string "Currency: " "EUR")))
    (ip-tax--record-transaction date desc (- amount) curr ip-tax-expense-account)))

;;;###autoload
(defun ip-tax-notify-income (&key client amount date currency description)
  "Вызывается извне (например, из ip-invoice) для записи ожидаемого дохода."
  (ip-tax--record-transaction
   (or date (format-time-string "%Y-%m-%d"))
   (or description (format "Income from %s" client))
   (if (stringp amount) (string-to-number amount) amount)
   (or currency "EUR")
   ip-tax-income-account))

;; —————————————————————————————————————
;; Опциональная интеграция с ip-invoice
;; —————————————————————————————————————

;; Если ip-invoice уже загружен — перехватываем финализацию
(when (featurep 'ip-invoice)
  (defun ip-tax--on-invoice-finalized (invoice-data)
    "Автоматически записывает доход при финализации инвойса."
    (let ((client-id (plist-get (plist-get invoice-data :client) :ID))
          (amount (plist-get invoice-data :total))
          (date (plist-get invoice-data :generated))
          (curr (plist-get invoice-data :currency)))
      (ip-tax-notify-income :client client-id
                            :amount amount
                            :date date
                            :currency curr
                            :description (format "Invoice %s" (plist-get invoice-data :invoice-id)))))

  ;; Патчим ip-invoice-generate (только если используется 'final)
  (advice-add 'ip-invoice-generate :after
              (lambda (&rest args)
                (when (and ip-tax-enabled (equal (nth 3 args) 'final))
                  (let ((invoice (apply #'ip-invoice-generate-data (butlast args))))
                    (ip-tax--on-invoice-finalized invoice))))))

(provide 'ip-tax)
;;; ip-tax.el ends here