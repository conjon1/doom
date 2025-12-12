;;; custom/hydraKeys.el --- Job Search Dashboard -*- lexical-binding: t; -*-

(require 'hydra)
(require 'denote)
(require 'consult)

(defun my/search-companies ()
  (interactive)
  ;; Search strictly inside the 'companies' subdirectory
  (consult-ripgrep (expand-file-name "companies" denote-directory) ""))

(defun my/search-contacts ()
  (interactive)
  ;; Search strictly inside the 'contacts' subdirectory
  (consult-ripgrep (expand-file-name "contacts" denote-directory) ""))

(defhydra hydra-jobs (:color blue :hint nil)
  "
  ^Job Search Command Center^   ^Status Filters^           ^Actions^
  -------------------------------------------------------------------------
  _h_: Open Charlotte Hub       _i_: Interviewing          _n_: New Standard Note
  _c_: Search Companies         _a_: Active Applications   _g_: Generate Company Note
  _p_: Search Contacts          _o_: Offers                _r_: Rename File
  _d_: Open Denote Dired        _R_: Rejections            _q_: Quit
  "
  ("h" (find-file "~/TheOrg/projects/JobSearch/CharlotteNC.org"))
  ("c" my/search-companies)
  ("p" my/search-contacts)
  ("d" (dired denote-directory))

  ("i" (consult-denote "interviewing"))
  ("a" (consult-denote "applied"))
  ("o" (consult-denote "offer"))
  ("R" (consult-denote "rejected"))

  ("n" denote)
  ("g" my/job-create-company-note)
  ("r" denote-rename-file)
  ("q" nil))

(map! :leader "j" #'hydra-jobs/body)

(provide 'hydraKeys)
