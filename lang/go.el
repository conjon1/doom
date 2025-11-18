;;; +lang/go.el -*- lexical-binding: t; -*-

;; Enable LSP automatically for Go files
(add-hook 'go-mode-hook #'lsp-deferred)

(after! go-mode
  ;; Use goimports instead of gofmt
  (setq gofmt-command "goimports")

  ;; Format before save
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (setq tab-width 3))

(after! lsp-mode
  (setq lsp-go-analyses
        '((unusedparams . t)
          (shadow . t)
          (unusedwrite . t)))
  (setq lsp-go-use-gofumpt t))
