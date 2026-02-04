;;; custom/denote-config.el -*- lexical-binding: t; -*-

(use-package! denote
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "denote/" org-directory))

  ;; --- GLOBAL SETTINGS ---
  (setq denote-prompts '(title keywords))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)

  ;; --- TEMPLATES ---
  (defun my/read-template (file)
    (let ((path (expand-file-name (concat "templates/" file) org-directory)))
      (if (file-exists-p path)
          (with-temp-buffer (insert-file-contents path) (buffer-string))
        "")))

  (setq denote-templates
        `((journal . ,(my/read-template "journal.org"))
          (contact . ,(my/read-template "contact.org"))
          (programming . ,(my/read-template "programming.org"))))

  ;; --- DYNAMIC HELPER (OPTIMIZED RIPGREP) ---
  (defun my/get-prop-values (prop)
    "Use Ripgrep to extract unique values for a specific Org property."
    (let ((cmd (format "rg -INoh \"^\\s*:%s:\\s+(.+)\" -r '$1' %s | sort | uniq"
                       prop denote-directory)))
      (split-string (shell-command-to-string cmd) "\n" t)))

  ;; --- CAPTURE WORKFLOWS ---

  ;; 1. QUICK NOTE (Title Only)
  (defun my/create-quick-note ()
    (interactive)
    (let ((denote-prompts '(title)))
      (call-interactively #'denote)))

  ;; 2. Programming NOTE (Dynamic History)

  (defun my/create-programming-note ()
    (interactive)
    (let* ((title (read-string "Topic: "))
          (langs (or (my/get-prop-values "Language") '("Go" "Emacs Lisp" "Bash")))
          (types (or (my/get-prop-values "Type") '("Snippet" "Concept" "Config")))
          (lang (completing-read "Language: " langs nil nil))
          (type (completing-read "Type: " types nil nil))
          ;; We pre-format the keywords as a list to avoid the extra prompt
          (keywords (list "dev" (downcase lang) (downcase type))))

      ;; 1. Create the note and capture the buffer it returns
      (let ((new-buffer (denote title keywords 'org nil nil 'programming)))

        ;; 2. Perform replacements inside that specific buffer
        (with-current-buffer new-buffer
          (save-excursion
            (goto-char (point-min))
            ;; Use 'fixedcase' in replace-match to prevent accidental capitalization
            (while (search-forward "{{LANG}}" nil t)
              (replace-match lang t t))
            (while (search-forward "{{TYPE}}" nil t)
              (replace-match type t t))
            (while (search-forward "{{LANG_SRC}}" nil t)
              (let ((src-lang (if (string= lang "Emacs Lisp") "elisp" (downcase lang))))
                (replace-match src-lang t t))))

        ;; 3. Optional: Save the buffer after modification
        (save-buffer)))))



  ;; 3. JOURNAL ENTRY
  (defun my/create-journal-entry ()
    (interactive)
    (let ((title (read-string "Journal Title: ")))
      ;; FIXED: Added 'nil' for the DATE argument.
      ;; Arg 5 is DATE (nil = now), Arg 6 is TEMPLATE ('journal)
      (denote title '("journal") 'org "journal" nil 'journal)))

  ;; 4. CONTACT NOTE
  (defun my/create-contact ()
    (interactive)
    (let ((name (read-string "Contact Name: ")))
      ;; FIXED: Added 'nil' for the DATE argument here as well.
      (denote name (denote-keywords-prompt '("network" "contact")) 'org "contacts" nil 'contact)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "{{.*?}}" nil t) (replace-match "")))))

  ;; Keybindings
  (map! :leader
        "n" #'hydra-denote/body))
;; Bind C-c C-c ONLY in Denote files
(add-hook 'org-mode-hook
          (lambda ()
            (when (denote-file-has-identifier-p (buffer-file-name))
              (local-set-key (kbd "C-c C-c") #'my/save-and-close-window))))
