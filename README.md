# IP Core - Freelance Management System for Emacs

A comprehensive Emacs-based system for managing freelance work, integrating task tracking, invoicing, and tax accounting through Org-mode.

## Features

### 📋 Core (`ip-core.el`)

- Client and company data management
- Org-mode based data storage
- Automatic caching with file watching
- Property-based service definitions

### 🐛 Debug (`ip-debug.el`)

- Unified logging system across all modules
- Module-specific debug buffers
- Configurable log levels
- Real-time debugging tools

### 🔗 Forgejo Integration (`ip-forgejo.el`)

- Import assigned issues as Org tasks
- Two-way synchronization
- Time tracking with automatic logging
- Multi-instance support
- NBS QR code generation for Serbian invoices

### 📄 Invoicing (`ip-invoice.el`)

- Automatic invoice generation from Org clock entries
- HTML output with customizable templates
- Serbian invoice format with payment slips
- Draft and final invoice states
- MOD-97 reference number generation

### 💰 Tax Tracking (`ip-tax.el`)

- Beancount integration for accounting
- Automatic transaction recording
- Income and expense tracking
- Integration with invoice finalization

## Installation

### Using straight.el (recommended for development)

```elisp
(use-package ip-core
  :straight (ip-core :type git
                     :host github
                     :repo "stillru/ip-core"
                     :files ("*.el"))
  :config
  (setq ip-org-directory "~/org/ip/")
  (ip-setup-files)
  (ip-validate-setup))

;; Load additional modules
(use-package ip-debug
  :straight (ip-debug :type git
                      :host github
                      :repo "stillru/ip-core"
                      :files ("ip-debug.el"))
  :after ip-core)

(use-package ip-forgejo
  :straight (ip-forgejo :type git
                        :host github
                        :repo "stillru/ip-core"
                        :files ("ip-forgejo.el"))
  :after ip-core
  :config
  (setq ip-forgejo-instances
        '(("work" . (("base-url" . "https://git.example.com/api/v1")
                     ("token" . "your-token-here")))))
  (ip-forgejo-mode 1))

(use-package ip-invoice
  :straight (ip-invoice :type git
                        :host github
                        :repo "stillru/ip-core"
                        :files ("ip-invoice.el"))
  :after ip-core
  :config
  (setq ip-invoice-template-file "~/templates/invoice.html"))

(use-package ip-tax
  :straight (ip-tax :type git
                    :host github
                    :repo "stillru/ip-core"
                    :files ("ip-tax.el"))
  :after (ip-core ip-invoice)
  :config
  (setq ip-tax-enabled t)
  (ip-tax-mode 1))
```

### Manual installation

```bash
git clone https://github.com/stillru/ip-core.git ~/.emacs.d/lisp/ip-core

# Add to your init.el:
(add-to-list 'load-path "~/.emacs.d/lisp/ip-core")
(require 'ip-core)
(require 'ip-invoice)
(require 'ip-forgejo)
(require 'ip-tax)
```

## Quick Start

1. **Initialize the system:**

    ```elisp
    M-x ip-setup-files
    ```

2. **Configure your company information:**
Edit `~/org/ip/company.org` with your business details.

3. **Add clients:**

    Edit `~/org/ip/clients.org` with client information and services.

4. **Track tasks:**
    Use `~/org/ip/tasks.org` with Org-mode clock entries.

5. **Generate invoices:**

    ```elisp
    M-x ip-invoice-month         ; Current month draft
    M-x ip-invoice-generate-interactive  ; Custom period
    ```

## Configuration

### Basic Setup

```elisp
;; Core settings
(setq ip-org-directory "~/org/ip/")
(setq ip-company-file "company.org")
(setq ip-clients-file "clients.org")
(setq ip-tasks-file "tasks.org")

;; Debug settings
(setq ip-debug-enabled t)
(setq ip-debug-module-buffers t)

;; Forgejo settings
(setq ip-forgejo-current-instance "work")
(setq ip-forgejo-sync-on-save t)

;; Invoice settings
(setq ip-invoice-default-exchange-rate 117.85)  ; EUR to RSD
(setq ip-invoice-include-payment-slip t)

;; Tax settings
(setq ip-tax-enabled t)
(setq ip-tax-auto-record-invoices t)
```

### Secure Token Storage

Use `auth-source` for Forgejo tokens:

```shell
# ~/.authinfo.gpg
machine git.example.com login ip-forgejo password your-token-here
```

```elisp
(let ((token (auth-source-pick-first-password 
              :host "git.example.com" 
              :user "ip-forgejo")))
  (setq ip-forgejo-instances
        `(("work" . (("base-url" . "https://git.example.com/api/v1")
                     ("token" . ,token))))))
```

## Key Bindings

The system uses `C-c i` as the main prefix:

```plain
C-c i o     - Show overview
C-c i r     - Refresh cache
C-c i v     - Validate setup

C-c i d s   - Show debug logs
C-c i d c   - Clear debug logs

C-c i f i   - Import Forgejo issues
C-c i f s   - Switch Forgejo instance
C-c i f p   - Push current entry
C-c i f r   - Refresh current issue

C-c i n m   - Generate invoice for current month
C-c i n g   - Generate invoice (interactive)

C-c i t i   - Record income
C-c i t e   - Record expense
C-c i t l   - Show ledger
```

## File Structure

```shell
~/org/ip/
├── company.org          # Company information
├── clients.org          # Client definitions with services
├── tasks.org            # Task tracking with clock entries
├── tax.beancount        # Tax ledger (if enabled)
└── invoices/
    ├── draft/           # Draft invoices
    └── final/           # Finalized invoices
```

## Documentation

- [Core Module Documentation](docs/ip-core.md)
- [Forgejo Integration Guide](docs/ip-forgejo.md)
- [Invoice Generation](docs/ip-invoice.md)
- [Tax Tracking](docs/ip-tax.md)

## Development

### Local Development Setup

```bash
git clone https://github.com/stillru/ip-core.git ~/Projects/ip-core
cd ~/Projects/ip-core

# Create test environment (not committed)
mkdir -p test/data test/templates test/invoices
```

```elisp
;; In your init.el for development
(use-package ip-core
  :straight (ip-core :type git
                     :local-repo "~/Projects/ip-core"
                     :files ("*.el")))
```

### Running Tests

```elisp
M-: (load-file "~/Projects/ip-core/test/test-config.el")
M-x ip-test-run-all
M-x ip-test-cleanup
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

**Important:** Never commit:

- Personal data or real invoices
- Authentication tokens
- Test data directories
- `.beancount` files with real transactions

## License

GPL-3.0 or later

## Author

Maintained by Stepan "stillru" Illichevskii <still.ru@gmail.com>

## Support

- GitHub Issues: <https://github.com/stillru/ip-core/issues>
- Documentation: <https://github.com/stillru/ip-core/wiki>
