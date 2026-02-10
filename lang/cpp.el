;;; lang/cpp.el -*- lexical-binding: t; -*-

(add-hook 'c-mode-common-hook #'lsp-deferred)

(after! c-mode
  (setq-default c-basic-offset 4)

  ;; Auto-format on save using LSP (Clangd)
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

;; Ensure .h files are treated as C++ by default (optional, usually safer)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
