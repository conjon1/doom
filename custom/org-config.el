;;; custom/org-config.el -*- lexical-binding: t; -*-

(after! org
  ;; --- Appearance ---

  ;; Indentation and visibility
  (setq org-startup-indented t)
  (setq org-adapt-indentation nil) ; Don't indent content
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-cycle-separator-lines 2)

  ;; Source blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-window-setup 'current-window)

  ;; --- Functionality ---

  ;; Logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)

  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  ;; Better exports
  (setq org-export-with-smart-quotes t)
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-with-sub-superscripts '{})

  ;; --- Agenda ---

  (setq org-agenda-start-on-weekday 1) ; Monday
  (setq org-agenda-span 'week)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-include-diary nil)

  ;; Performance: Only inherit tags when needed
  (setq org-agenda-use-tag-inheritance nil)

  ;; --- Archive ---

  (setq org-archive-location
        (concat (expand-file-name "archive/" org-directory)
                "archive_%s::"))

  ;; --- Links ---

  (setq org-link-keep-stored-after-insertion t)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(provide 'org-config)
