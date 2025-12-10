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
        "n" #'org-capture                      ; SPC n n -> Main Capture Menu
        "N" #'denote                           ; SPC n N -> Create separate file (Standard)
        "x" #'my/denote-extract-subtree        ; SPC n x -> Extract to flat file (Old)
        "S" #'my/splinter-subtree-to-hierarchy ; SPC n S -> Splinter to Folder Hierarchy (New)
        "R" #'my/reintegrate-file              ; SPC n R -> Re-integrate/Defrost file (New)
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
  "Copy subtree to new file (Flat Fork)."
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

(defun my/splinter-subtree-to-hierarchy ()
  "Move the current subtree to a new file mirroring the hierarchy, leaving a link."
  (interactive)
  (unless (org-at-heading-p) (user-error "Not at a heading!"))

  (let* ((title (org-get-heading t t t t))
         ;; 1. Get the hierarchy (e.g., "Programming" -> "Golang")
         (hierarchy (mapcar #'org-no-properties (org-get-outline-path)))
         ;; 2. Construct the target directory: ~/MonolithicNotes/Programming/Golang/
         (target-dir (file-name-as-directory
                      (expand-file-name (mapconcat #'identity hierarchy "/")
                                        denote-directory)))
         (tags (org-get-tags))
         ;; 3. Create the unique filename using Denote standards
         (new-file (denote-format-file-name
                    target-dir
                    (denote-get-identifier)
                    (denote-sluggify title)
                    (denote-sluggify-keywords tags)
                    ".org")))

    ;; 4. Create the directory if it doesn't exist
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))

    ;; 5. Create the new file with the content
    (org-cut-subtree)
    (with-temp-file new-file
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format "#+FILETAGS: %s\n\n" (mapconcat #'identity tags ":")))
      (org-yank))

    ;; 6. Leave the link behind in the Monolith
    (insert (format "*** [[file:%s][%s]]\n"
                    (file-relative-name new-file default-directory)
                    title))
    (save-buffer)
    (message "Splintered '%s' to %s" title target-dir)))

(defun my/reintegrate-file ()
  "Replace a file link at point with the file's content (delete external file)."
  (interactive)
  (let ((context (org-element-context)))
    ;; Ensure we are on a link
    (unless (eq (org-element-type context) 'link)
      (user-error "Cursor is not on a link!"))

    (let* ((path (org-element-property :path context))
           (abs-path (expand-file-name path))
           (content ""))

      ;; Safety checks
      (unless (file-exists-p abs-path)
        (user-error "Linked file does not exist: %s" abs-path))

      ;; 1. Read the file content
      (with-temp-buffer
        (insert-file-contents abs-path)
        ;; Remove file-level properties (TITLE, DATE, FILETAGS, etc.)
        (goto-char (point-min))
        (delete-matching-lines "^#\\+")
        (setq content (buffer-string)))

      ;; 2. Delete the link heading in the Monolith
      (org-back-to-heading)
      (delete-region (line-beginning-position) (line-end-position))

      ;; 3. Insert the content (restoring the original subtree)
      (insert content)
      (delete-blank-lines)

      ;; 4. Prompt to delete the external file
      (when (yes-or-no-p (format "Re-integrated content. Delete external file '%s'? " (file-name-nondirectory abs-path)))
        (delete-file abs-path)
        (message "File deleted."))

      ;; 5. Fold the subtree to keep things tidy
      (org-cycle))))

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
