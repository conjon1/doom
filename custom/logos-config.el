;;; custom/logos-config.el -*- lexical-binding: t; -*-

(use-package! logos
  :config
  ;; Appearance settings
  (setq logos-outlines-are-plainer t)
  (setq logos-hide-mode-line t)
  (setq logos-hide-buffer-boundaries t)
  (setq logos-buffer-read-only nil)
  (setq logos-olivetti t) ; Integrate with olivetti

  ;; Page delimiters (what counts as a "page")
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;+ ")
          (org-mode . "^\\*+ +")
          (markdown-mode . "^#+ +")
          (t . ,(or outline-regexp logos-page-delimiter))))

  ;; Keybindings
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim))

  ;; Optional: Hook for customizations when entering focus mode
  (add-hook 'logos-focus-mode-hook
            (lambda ()
              (if logos-focus-mode
                  (progn
                    ;; Entering focus mode
                    (text-scale-increase 0))
                ;; Exiting focus mode
                (text-scale-increase 0)))))

(provide 'logos-config)
