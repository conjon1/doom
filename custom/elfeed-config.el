;;; custom/elfeed-config.el -*- lexical-binding: t; -*-

(after! elfeed
  ;; This tells elfeed to look for a file named "elfeed.org"
  ;; inside your org-directory ("~/NoteSphere/")
  (setq elfeed-feeds (list (concat org-directory "elfeed.org")))

  ;; You can add other elfeed customizations here. For example:
  ;; Set the default filter to show all unread entries
  (setq elfeed-search-filter "@unread")

  ;; Set the width for entry titles
  (setq elfeed-search-title-max-width 100))
