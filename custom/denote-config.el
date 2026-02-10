;;; -*- lexical-binding: t; -*-

(after! denote
  ;; 1. Setup Directories
  (setq denote-directory (expand-file-name "~/TheOrg/denote/"))
  (defvar my-template-dir (expand-file-name "templates/" "~/TheOrg/")
    "Directory containing Org mode template files.")

  ;; 2. Template Processor
  ;; This function handles the {{placeholder}} replacement logic.
  (defun my/read-template-and-fill-placeholders (template-file)
    (with-temp-buffer
      (insert-file-contents template-file)
      (goto-char (point-min))
      (while (re-search-forward "{{\\([^}]+\\)}}" nil t)
        (let* ((placeholder (match-string 1))
               (value (read-string (format "Value for %s: " placeholder))))
          (replace-match value t t)))
      (buffer-string)))

  ;; 3. Dynamic Template Loader
  ;; Refreshes denote-templates by scanning the directory.
  (defun my/refresh-denote-templates ()
    (when (file-directory-p my-template-dir)
      (setq denote-templates
            (mapcar
             (lambda (file)
               (cons
                (intern (file-name-base file))
                ;; The value is a function that returns the template string.
                (lambda () (my/read-template-and-fill-placeholders file))))
             (directory-files my-template-dir t "\\.org$")))))

  ;; 4. Custom Command
  ;; Implemented using the manual's recommended 'denote-add-prompts'.
  (defun my/create-note-from-template ()
    "Create a note by specifically prompting for a template."
    (interactive)
    (my/refresh-denote-templates)
    ;; Appends 'template' to the standard prompts.
    (let ((denote-prompts (denote-add-prompts '(template))))
      (call-interactively #'denote)))

  ;; 5. Keybinding
  (map! :leader
        :desc "New template note"
        "n n" #'my/create-note-from-template))
