;;; custom/lin-config.el -*- lexical-binding: t; -*-

(use-package! lin
  :config
  ;; Line highlighting style
  (setq lin-face 'lin-blue) ; Options: lin-blue, lin-cyan, lin-green, lin-yellow, lin-red, lin-magenta

  ;; Modes where lin should be active
  ;; These are modes where line selection is the primary interaction
  (setq lin-mode-hooks
        '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode-hook
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))

  ;; Enable globally
  (lin-global-mode 1))

(provide 'lin-config)
