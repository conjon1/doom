;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Connal McInnis"
      user-mail-address "connal.jmcinnis@proton.me")


;; --- Settings ---

(setq org-directory "~/TheOrg/")
(setq org-log-done 'time)
(setq org-agenda-files
      (directory-files-recursively "~/TheOrg/" "\\.org$"))


;; --- Modules ---
(load! "custom/ui-config.el")
(load! "lang/go.el")
(load! "custom/treesit.el")
(load! "custom/denote-config.el")
;; (load! "custom/org-roam-config.el")
(load! "custom/hydraKeys.el")


;; --- Other ---

(setq doom-theme 'doom-oceanic-next)
(after! doom-themes
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13)))

(set-frame-parameter (selected-frame) 'alpha '(95 . 75))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))

(savehist-mode 1)
(setq display-line-numbers-type 'relative)
