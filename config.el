;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Connal McInnis"
      user-mail-address "connal.jmcinnis@proton.me")


;; --- Core Settings ---

(setq org-directory "~/TheOrg/")
(setq org-log-done 'time)

;; Recursive agenda files (consider moving to org-config.el)
(setq org-agenda-files
      (directory-files-recursively "~/TheOrg/" "\\.org$"))


;; --- Load Custom Modules ---

(load! "custom/ui-config.el")
(load! "custom/org-config.el")
(load! "custom/denote-config.el")
(load! "custom/elfeed-config.el")
(load! "custom/pulsar-config.el")
(load! "custom/logos-config.el")
(load! "custom/fontaine-config.el")
(load! "custom/lin-config.el")                 ; ← Line highlighting
(load! "custom/spacious-padding-config.el")    ; ← Breathing room

;; Language-specific configs
(load! "lang/go.el")
(load! "lang/python.el")
(load! "lang/cpp.el")
(load! "lang/rust.el")
(load! "custom/treesit.el")


;; --- Theme & Appearance ---
;;(setq doom-theme '
;;(setq doom-theme 'doom-vibrant)
;;(setq doom-theme 'doom-palenight)
;;(setq doom-theme 'doom-oceanic-next)
;; (setq doom-theme 'modus-vivendi-tinted) ; or modus-operandi-tinted for light
(setq doom-theme 'doom-monokai-pro)
;;(setq doom-theme 'doom-moonlight)
;; Font (moved to fontaine-config.el for better management)
;; But Doom still needs this for initial setup
(after! doom-themes
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13)))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 75))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))


;; --- General Settings ---

(savehist-mode 1)
(setq history-length 25)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)

(setq display-line-numbers-type 'relative)


;; --- Keybindings ---

(map! :leader
      ;; Denote operations
      :prefix ("n" . "notes")
      "n" #'denote
      "f" #'consult-denote
      "i" #'denote-link
      "b" #'denote-backlinks
      "r" #'denote-rename-file
      "t" #'denote-rename-file-keywords
      "d" #'(lambda () (interactive) (dired denote-directory))

      ;; Toggle various modes
      :prefix ("t" . "toggle")
      "f" #'logos-focus-mode       ; Focus mode with logos
      "o" #'olivetti-mode          ; Just olivetti
      "p" #'variable-pitch-mode    ; Variable pitch fonts
      "s" #'spacious-padding-mode  ; Toggle padding

      ;; Font presets (fontaine)
      :prefix ("F" . "fonts")
      "r" #'(lambda () (interactive) (fontaine-set-preset 'regular))
      "l" #'(lambda () (interactive) (fontaine-set-preset 'large))
      "p" #'(lambda () (interactive) (fontaine-set-preset 'presentation))
      "s" #'(lambda () (interactive) (fontaine-set-preset 'small))
      "f" #'fontaine-set-preset) ; Interactive selection
