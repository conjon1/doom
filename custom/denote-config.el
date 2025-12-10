;;; custom/denote-config.el -*- lexical-binding: t; -*-

(use-package! denote
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/MonolithicNotes/"))
  (setq denote-infer-keywords t)
  (setq denote-known-keywords nil)
  (setq denote-prompts '(title keywords))

  ;; KEYBINDINGS
  (map! :leader
        :prefix "n"
        "n" #'org-capture              ; SPC n n -> Main Capture Menu
        "N" #'denote                   ; SPC n N -> Create separate file (Old way)
        "x" #'my/denote-extract-subtree
        "r" #'denote-rename-file
        "l" #'denote-link-or-create
        "s" #'consult-grep))

;; --- STATE MANAGEMENT (Dashboard.org) ---
(after! org
  ;; Agenda looks at your "State" file, not your notes
  (setq org-agenda-files '("~/MonolithicNotes/Dashboard.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(after! elfeed
  ;; Elfeed looks for feeds in your Dashboard
  (setq rmh-elfeed-org-files (list "~/MonolithicNotes/Dashboard.org")))

;; --- CUSTOM TOOLS ---
(defun my/denote-extract-subtree ()
  "Copy subtree to new file (Fork)."
  (interactive)
  (unless (org-at-heading-p) (user-error "Not at a heading!"))
  (let* ((heading-text (org-get-heading t t t t))
         (tags (org-get-tags))
         (title (read-string "New File Title: " heading-text)))
    (org-copy-subtree)
    (denote title tags)
    (goto-char (point-max)) (insert "\n") (org-yank)
    (save-buffer)
    (message "Copied '%s' to new note!" title)))

(defun my/select-denote-tags ()
  (let ((tags (completing-read-multiple
               "Select Tags: " (denote-keywords))))
    (if tags (format ":%s:" (mapconcat #'identity tags ":")) "")))

;; --- CAPTURE TEMPLATES ---
(after! org
  (setq org-capture-templates
        (append org-capture-templates
                '(
                  ;; 1. KNOWLEDGE -> Monolith.org
                  ("n" "Note (Knowledge)" entry
                   (file+headline "~/MonolithicNotes/Monolith.org" "Inbox")
                   "* %^{Title} %(my/select-denote-tags)\n:PROPERTIES:\n:Created: %U\n:END:\n%?"
                   :prepend t :empty-lines 1)

                  ;; 2. ACTION -> Dashboard.org
                  ("t" "Task / Todo" entry
                   (file+headline "~/MonolithicNotes/Dashboard.org" "Inbox")
                   "* TODO %^{Task} \nSCHEDULED: %^t\n:PROPERTIES:\n:Created: %U\n:END:\n%?"
                   :prepend t :empty-lines 1)

                  ("p" "Project Log" entry
                   (file+headline "~/MonolithicNotes/Dashboard.org" "Active Projects")
                   "* %U %^{Update} :log:\n%?"
                   :prepend t :empty-lines 1)
                  ))))
