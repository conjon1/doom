;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Connal McInnis"
      user-mail-address "connal.jmcinnis@proton.me")

;; --- Modules ---
(load! "custom/denote-config.el")
(load! "custom/ui-config.el")
(load! "custom/elfeed-config.el")
(load! "lang/go.el")
(load! "custom/treesit.el")
;; (load! "custom/org-roam-config.el")
;; (load! "custom/hydraOrg.el")

;; General Settings ---
(setq doom-theme 'doom-oceanic-next)

(after! doom-themes
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13)))

(set-frame-parameter (selected-frame) 'alpha '(95 . 75))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))

(setq org-directory "~/MonolithicNotes/")
(setq org-log-done 'time)
(savehist-mode 1)
(setq display-line-numbers-type 'relative)
