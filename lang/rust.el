;;; lang/rust.el -*- lexical-binding: t; -*-

;; Hook into rustic-mode (recommended for Doom) or standard rust-mode
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'rustic-mode-hook #'lsp-deferred)

(after! lsp-rust
  ;; Enable rust-analyzer features
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t))

;; Auto-format on save is usually handled by rustic-mode automatically.
;; If using standard rust-mode, uncomment the following:
;; (add-hook 'before-save-hook #'lsp-format-buffer nil t)
