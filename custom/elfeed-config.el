;;; custom/elfeed-config.el -*- lexical-binding: t; -*-

(after! elfeed
  ;; This tells elfeed to look for a file named "elfeed.org"
  ;; inside your org-directory ("~/NoteSphere/")
  ;;
  ;;(setq elfeed-feeds (list (concat org-directory "elfeed.org")))

  ;; You can add other elfeed customizations here. For example:
  ;; Set the default filter to show all unread entries
  (setq elfeed-search-filter "@unread")


  (setq elfeed-search-title-max-width 100))

(use-package! elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-size 0.5)) ; Split window 50/50


(after! elfeed
  (setq elfeed-search-face-alist
        '(
          (unread . foreground-color "white"))
          (go     . (:foreground "#51a4b4" :weight bold)) ; Go Blue
          (k8s    . (:foreground "#326ce5" :weight bold)) ; K8s Blue
          (sec    . (:foreground "#e06c75" :weight bold)) ; Red for Security
          (hack   . (:foreground "#98c379" :weight bold)) ; Green for Hacking
          (emacs  . (:foreground "#c678dd" :weight bold)) ; Purple for Emacs
          (news   . (:foreground "#56b6c2"))              ; Cyan for News
          )))
