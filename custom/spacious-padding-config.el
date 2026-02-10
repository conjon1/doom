;;; custom/spacious-padding-config.el -*- lexical-binding: t; -*-

(use-package! spacious-padding
  :config
  ;; Padding/spacing values
  (setq spacious-padding-widths
        '(:internal-border-width 15
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 30
          :scroll-bar-width 8
          :fringe-width 8))

  ;; Optional: Subtle background for mode-line
  (setq spacious-padding-subtle-mode-line
        '(:mode-line-active default
          :mode-line-inactive vertical-border))

  ;; Enable globally
  (spacious-padding-mode 1))

;; Optional: Hook to refresh after theme changes
(add-hook 'modus-themes-after-load-theme-hook #'spacious-padding-mode)

(provide 'spacious-padding-config)
