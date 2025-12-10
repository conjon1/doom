;;; custom/ui-config.el -*- lexical-binding: t; -*-

;; --- Zen Mode (Zoomed & Centered) ---
(use-package! olivetti
  :hook (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 120)
  (setq olivetti-style 'fancy)

  ;; Auto-zoom font when entering Zen mode
  (add-hook 'olivetti-mode-hook
            (lambda ()
              (if olivetti-mode
                  (text-scale-increase 1.7)
                (text-scale-increase 0)))))

;; --- Typography (Reading Fonts) ---
(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-drawer 'org-special-keyword 'org-property-value
            'org-block 'org-block-begin-line 'org-block-end-line
            'org-meta-line 'org-document-info-keyword
            'org-code 'org-verbatim))

;; --- Clean UI (Hide Markers) ---
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t))


;; --- Bullet Points  ---
(use-package! org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-remove-leading-stars t
        org-superstar-special-todo-items nil)

  ;; The "Gravity" Orbit Theme
  (setq org-superstar-headline-bullets-list '("●" "◉" "◎" "○" "◌" "⋅")))
