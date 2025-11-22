;;; lang/go.el -*- lexical-binding: t; -*-

;; --- Go Configuration ---
(add-hook 'go-mode-hook #'lsp-deferred)

(after! go-mode
  (setq tab-width 4)

  (defun +go/lsp-format-and-organize ()
    (when (bound-and-true-p lsp-mode)
      (lsp-organize-imports)
      (lsp-format-buffer)))

  (add-hook 'before-save-hook #'+go/lsp-format-and-organize nil t))

(after! lsp-mode
  (setq lsp-go-use-gofumpt t)

  (setq lsp-go-analyses
        '((unusedparams . t)
          (shadow . t)
          (unusedwrite . t)
          (nilness . t)
          (useany . t)
          (fieldalignment . t))))

;; --- Templ Configuration ---

(use-package! templ-ts-mode
  :mode "\\.templ\\'"
  :config
  (add-hook 'templ-ts-mode-hook #'lsp-deferred)

  ;; Optional: Set indentation width if different from default
  ;; (setq templ-ts-mode-indent-offset 4)
  )

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(templ-ts-mode . "templ"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("templ" "lsp"))
                    :activation-fn (lsp-activate-on "templ")
                    :server-id 'templ)))
