;;; custom/org-roam-config.el -*- lexical-binding: t; -*-
(after! org-roam :config
  (setq org-roam-directory (file-truename "~/NoteSphere/roam/"))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode 1)
  (setq org-roam-completion-everywhere t)
(setq org-roam-capture-templates
      '(("n" "Note")
        ("nd" "Default Note" plain
         "%?"
         :if-new (file+head "%(expand-file-name (format \"notes/%s.org\" (org-id-new)) org-roam-directory)"
                          "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
         :unnarrowed t)

        ("nd" "Default Note" plain
         "%?"
         :if-new (file+head "%(expand-file-name (format \"notes/%s.org\" (org-id-new)) org-roam-directory)"
                          "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
         :unnarrowed t)

        ("np" "Programming Note" plain
         "%?"
         :if-new (file+head "%(expand-file-name (format \"notes/%s.org\" (org-id-new)) org-roam-directory)"
                          "%(let* (
                                 (title (read-string \"Title: \"))
                                 (lang (read-string \"Language (e.g., 'go', 'python', 'C'): \"))

                                 (project-dir (expand-file-name \"projects\" org-roam-directory))

                                 (project-files (if (file-directory-p project-dir)
                                                    (directory-files project-dir nil \"^[^.].*\\.org$\")
                                                  ;; Otherwise, just use 'nil' (an empty list)
                                                  nil))

                                 (project-slugs (mapcar #'file-name-sans-extension project-files))
                                 (all-choices (cons \"--NONE--\" project-slugs))
                                 (proj-slug (completing-read \"Project (select --NONE-- to skip): \" all-choices nil t))

                                 (tags (concat \"programming \" lang))
                                 (proj-link (if (string-equal proj-slug \"--NONE--\")
                                                \"\"
                                                (format \"\n* Project\n[[roam:%s]]\n\" proj-slug)))
                                 (date-str (format-time-string \"[%Y-%m-%d %H:%M]\"))
                                 )

                            (let ((raw-string (format (concat \"#+title: %s\n#+date: %s\n#+filetags: %s\n\"
                                                              \"%s\n\"
                                                              \"* Notes\n\")
                                                      title date-str tags proj-link)))

                              (replace-regexp-in-string \"%\" \"%%\" raw-string))
                            )")
         :unnarrowed t)

        ("nl" "Note (with Link)" plain
         "%?"
         :if-new (file+head "%(expand-file-name (format \"notes/%s.org\" (org-id-new)) org-roam-directory)"
                          "%(let* (
                                 (title (read-string \"Title: \"))

                                 (project-dir (expand-file-name \"projects\" org-roam-directory))
                                 (project-files (if (file-directory-p project-dir)
                                                    (directory-files project-dir nil \"^[^.].*\\.org$\")
                                                  nil))
                                 (project-slugs (mapcar #'file-name-sans-extension project-files))
                                 (all-choices (cons \"--NONE--\" project-slugs))
                                 (proj-slug (completing-read \"Project (select --NONE-- to skip): \" all-choices nil t))

                                 (tags \"\")
                                 (proj-link (if (string-equal proj-slug \"--NONE--\")
                                                \"\"
                                                (format \"\n* Project\n[[roam:%s]]\n\" proj-slug)))
                                 (date-str (format-time-string \"[%Y-%m-%d %H:%M]\"))
                                 )

                            (let ((raw-string (format (concat \"#+title: %s\n#+date: %s\n#+filetags: %s\n\"
                                                              \"%s\n\n\")
                                                      title date-str tags proj-link)))

                              (replace-regexp-in-string \"%\" \"%%\" raw-string))
                            )")
         :unnarrowed t)
        
         ("b" "Book" plain
         (file "~/NoteSphere/roam/Templates/bookTemplate.org")
         :if-new (file+head "%(expand-file-name (format \"literature/%s.org\" (read-string \"Slug: \")) org-roam-directory)"
                          "")
         :unnarrowed t)

        ("p" "Project" plain
         (file "~/NoteSphere/roam/Templates/projectTemplate.org")
         :if-new (file+head "%(expand-file-name (format \"projects/%s.org\" (read-string \"Slug: \")) org-roam-directory)"
                          "")
         :unnarrowed t)

        ("w" "Writing" plain
         (file "~/NoteSphere/roam/Templates/writingTemplate.org")
         :if-new (file+head "%(expand-file-name (format \"writings/%s.org\" (read-string \"Slug: \")) org-roam-directory)"
                          "")
         :unnarrowed t)

        ("j" "Jobs")
        ("ji" "Job: Interview" plain
         (file "~/NoteSphere/roam/Templates/interveiwTemplate.org")
         :if-new (file+head "%(expand-file-name (format \"interviews/%s.org\" (read-string \"Slug: \")) org-roam-directory)"
                          "")
         :unnarrowed t)

        ("jc" "Job: Contact" plain
         (file "~/NoteSphere/roam/Templates/contactTemplate.org")
         :if-new (file+head "%(expand-file-name (format \"contacts/%s.org\" (read-string \"Slug: \")) org-roam-directory)"
                          "")
         :unnarrowed t)

        ("jl" "Job: LAMP Company" plain
         (file "~/NoteSphere/roam/Templates/companyTemplate.org")
         :if-new (file+head "%(expand-file-name (format \"companies/%s.org\" (read-string \"Slug: \")) org-roam-directory)"
                          "")
         :unnarrowed t)
        ))
)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(provide 'org-roam-config)
