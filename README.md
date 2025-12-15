# IP Management System for Emacs

A modular system for freelancers and sole proprietors (IP) to manage:

- clients & contracts (`ip-core`)
- time tracking via Org-mode (`ip-forgejo`, `ip-invoice`)
- invoicing (`ip-invoice`)
- tax & cash flow tracking via Beancount (`ip-tax`)

All data lives in plain-text Org files → fully version-controllable.

---

## 🔧 Quickstart (Local Test Setup)

1. **Clone or place all `.el` files** into `~/.emacs.d/lisp/`:
   - `ip-core.el`
   - `ip-invoice.el`
   - `ip-forgejo.el` (optional)
   - `ip-debug.el` (optional)
   - `ip-tax.el` ← новый модуль

2. **Create a test config** `~/.emacs.d/ip-test-setup.el`:

    ```elisp
    (setq ip-org-directory "~/org/ip-test/")
    (add-to-list 'load-path "~/.emacs.d/lisp/")
    (require 'ip-core)
    (require 'ip-invoice)
    (require 'ip-tax)
    (setq ip-tax-enabled t)
    (ip-setup-files) ; creates templates
    ```

3. **Launch test Emacs**:

    ```shell
    emacs -Q -l ~/.emacs.d/ip-test-setup.el
    ```

4. **Explore**:

    ```emacs
    M-x ip-show-overview
    ```

    ```emacs
    M-x ip-invoice-month
    ```

    ```emacs
    M-x ip-tax-record-income
    ```

    Check ~/org/ip-test/ for generated files

5. **View ledger**:

    Open `~/org/ip-test/tax.beancount` — valid Beancount format
    Use Fava for web UI

6. **Modules**

    | Module     | Purpose                                    |
    | ---------- | ------------------------------------------ |
    | ip-core    | Loads company/client data from Org files   |
    | ip-invoice | Generates HTML invoices from clocked time  |
    | ip-forgejo | Syncs Git/Forgejo issues as Org tasks      |
    | ip-tax     | Tracks income/expenses in Beancount ledger |
    | ip-debug   | Unified logging (optional)                 |

7. **Requirements**:

- Emacs 27+
- org-mode (9.0+)
- *Optional:* beancount.el (for syntax, but not required)
- *Optional:* mustache.el (for invoices)
