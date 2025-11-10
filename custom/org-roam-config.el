;;; custom/org-roam-config.el -*- lexical-binding: t; -*-
(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/NoteSphere/roam/"))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode 1)
  (setq org-roam-completion-everywhere t)

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "%(expand-file-name (format \"notes/%s.org\" (org-id-new)) org-roam-directory)"
                          "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
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
(provide 'org-roam-config)
