;;; Custom hydraKeys.el -*- lexical-binding: t; -*-

(require 'hydra)
(require 'denote)
(require 'consult)

;; --- Helpers ---
(defun my/search-all () (interactive) (consult-ripgrep denote-directory ""))

(defun my/run-in-split (command)
  "Splits the window to the right and runs the command in the new window."
  (interactive)
  (select-window (split-window-right))
  (balance-windows)
  (call-interactively command))

(defun my/save-and-close-window ()
  (interactive)
  (save-buffer)
  (delete-window))

(defhydra hydra-denote (:color blue :columns 3)
  "Denote Menu"
  ;; Column 1: Capture (Wrapped in split)
  ("n" (my/run-in-split #'my/create-quick-note) "Quick Note")
  ("N" (my/run-in-split #'denote) "Note + Tags")
  ("p" (my/run-in-split #'my/create-programming-note) "Code Note")
  ("j" (my/run-in-split #'my/create-journal-entry) "Journal Entry")
  ("C" (my/run-in-split #'my/create-contact) "Contact Note")

  ;; Column 2: Search
  ("f" consult-denote "Find Title")
  ("s" my/search-all "Search Text")
  ("d" (dired denote-directory) "Open Dired")

  ;; Column 3: Actions
  ("b" denote-backlinks "Backlinks")
  ("r" denote-rename-file "Rename")
  ("t" denote-rename-file-keywords "Manage Tags")
  ("l" denote-link "Insert Link")
  ("q" nil "Quit" :exit t))

(provide 'hydraKeys)
