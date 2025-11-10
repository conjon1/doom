;;; hydraKeys.el --- Custom Hydras for my workflow -*- lexical-binding: t; -*-

(require 'hydra)
(require 'org-roam-node)


(defun my/find-job-hub ()
  "Open the main job search project file."
  (interactive)
  (find-file (expand-file-name "~/NoteSphere/roam/projects/job search.org")))

(defun my/find-job-interviews ()
  "Find Org Roam notes tagged 'interview'."
  (interactive)
  (org-roam-node-find nil "#interview "))

(defun my/find-job-contacts ()
  "Find Org Roam notes tagged 'contact'."
  (interactive)
  (org-roam-node-find nil "#contacts "))

(defun my/find-job-lamp-list ()
  "Find Org Roam notes tagged 'lamp_list'."
  (interactive)
  (org-roam-node-find nil "#lamp_list "))

(defun my/find-job-shade-analysis ()
  "Search all roam notes for the 'SHADE Analysis' heading."
  (interactive)
  (consult-ripgrep org-roam-directory "SHADE Analysis"))


(defhydra hydra-jobs (:color blue :hint nil)
  "
  ^Job Dashboard
  -----------------
  _j_: Jobs Hub
  _i_: Interviews
  _c_: Contacts
  _l_: LAMP List
  _s_: SHADE Analysis
  _q_: Quit
  "
  ("j" my/find-job-hub)
  ("i" my/find-job-interviews)
  ("c" my/find-job-contacts)
  ("l" my/find-job-lamp-list)
  ("s" my/find-job-shade-analysis)
  ("q" nil))

(map! :leader
      :prefix "n"
      "j" #'hydra-jobs/body)


(provide 'hydraKeys)
