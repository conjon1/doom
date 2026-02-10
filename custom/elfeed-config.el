;;;; custom/elfeed-config.el -*- lexical-binding: t; -*-

(use-package! elfeed
  :config
  ;; Feed sources
  (setq elfeed-feeds
        '(;; Emacs
          ("https://protesilaos.com/codelog.xml" emacs prot)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://sachachua.com/blog/category/emacs-news/feed/" emacs news)

          ;; Tech news (add your own)
          ;; ("https://example.com/feed" tech)
          ))

  ;; UI settings
  (setq elfeed-search-filter "@1-week-ago +unread")
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)

  ;; Date format
  (setq elfeed-search-date-format '("%Y-%m-%d" 10 :left)))

;; YouTube integration
(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup)
  (setq elfeed-tube-auto-save-p nil) ; Don't save watched state automatically
  (setq elfeed-tube-auto-fetch-p t)) ; Fetch video info automatically

(provide 'elfeed-config);; custom/elfeed-config.el -*- lexical-binding: t; -*-
