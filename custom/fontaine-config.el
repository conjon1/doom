;;; custom/fontaine-config.el -*- lexical-binding: t; -*-

(use-package! fontaine
  :config
  ;; Define your font presets
  (setq fontaine-presets
        '((regular
           :default-family "JetBrainsMono Nerd Font"
           :default-height 130
           :variable-pitch-family "ETBembo") ; Or any font you prefer

          (large
           :default-family "JetBrainsMono Nerd Font"
           :default-height 150
           :variable-pitch-family "ETBembo")

          (presentation
           :default-family "JetBrainsMono Nerd Font"
           :default-height 170
           :variable-pitch-family "ETBembo"
           :bold-weight extrabold)

          (small
           :default-family "JetBrainsMono Nerd Font"
           :default-height 110
           :variable-pitch-family "ETBembo")))

  ;; Set the default preset
  (fontaine-set-preset 'regular)

  ;; Persist font preset across sessions
  (fontaine-mode 1)

  ;; Optional: Save the latest preset
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(provide 'fontaine-config)
