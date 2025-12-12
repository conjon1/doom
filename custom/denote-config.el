;;; custom/denote-config.el -*- lexical-binding: t; -*-

(use-package! denote
  :hook (dired-mode . denote-dired-mode)
  :config
  ;; --- 1. CORE SETTINGS ---
  (setq denote-directory (expand-file-name "denote/" org-directory))

  ;; Ensure we prompt for subdirectory to keep things organized
  (setq denote-prompts '(title keywords subdirectory template))

  ;; --- 2. TEMPLATE SYSTEM (READ FROM FILES) ---
  (defun my/read-template-file (filename)
    "Read the content of FILENAME in the org-directory/templates folder."
    (let ((filepath (expand-file-name (concat "templates/" filename) org-directory)))
      (if (file-exists-p filepath)
          (with-temp-buffer
            (insert-file-contents filepath)
            (buffer-string))
        (message "Template file not found: %s" filepath)
        "")))

  ;; Define templates by reading external files
  ;; Note: We use backtick (`) and comma (,) to evaluate the function call immediately
  (setq denote-templates
        `((company . ,(my/read-template-file "company-profile.org"))
          (contact . ,(my/read-template-file "contact.org"))
          (journal . "* Daily Log\n\n")))

  ;; --- 3. WORKFLOW: AUTOMATIC NOTE LINKING ---
  (defun my/job-create-company-note ()
    "Turn the current line's company name into a linked Denote note in the 'companies' subdir."
    (interactive)
    (let* ((line-text (thing-at-point 'line t))
           ;; Extract text between ** ** or just clean the line
           (clean-name (if (string-match "\\*\\*\\(.*?\\)\\*\\*" line-text)
                           (match-string 1 line-text)
                         (string-trim (replace-regexp-in-string "^\s*-\s*\\[.\\]\s*" "" line-text))))
           ;; Define the new file attributes
           (title clean-name)
           (keywords '("jobsearch" "target"))
           (subdir "companies") ;; Force into the 'companies' folder
           (template 'company)) ;; Use the company template

      ;; Create the note using Denote's internal API
      (denote title keywords 'org subdir template)

      ;; Save the new note buffer so we can link to it
      (save-buffer)

      ;; Create the link string
      (let ((new-link (format "[[denote:%s][%s]]"
                              (denote-retrieve-filename-identifier (buffer-file-name))
                              title)))

        ;; Go back to the original buffer
        (other-window 1)
        ;; Replace the company name with the Org Link
        (beginning-of-line)
        (if (search-forward title (line-end-position) t)
            (replace-match new-link))

        ;; Switch back to the new note to start editing
        (other-window 1))))

  ;; --- 4. KEYBINDINGS ---
  (map! :leader
        :prefix "n"
        "n" #'denote                   ; Create note (standard)
        "o" #'denote-open-or-create    ; Search or create
        "r" #'denote-rename-file       ; Rename current file
        "g" #'my/job-create-company-note)) ; "Generate" Company Note
