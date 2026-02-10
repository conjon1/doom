;;; lang/python.el -*- lexical-binding: t; -*-

(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'python-ts-mode-hook #'lsp-deferred)

(after! python
  (defun +python/lsp-format-and-organize ()
    (when (bound-and-true-p lsp-mode)
      (lsp-organize-imports)
      (lsp-format-buffer)))

  (add-hook 'before-save-hook #'+python/lsp-format-and-organize nil t))
