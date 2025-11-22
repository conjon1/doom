;;; custom/treesit.el -*- lexical-binding: t; -*-
(after! treesit
  (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter/" doom-cache-dir))

  (setq treesit-language-source-alist
        '((go "https://github.com/tree-sitter/tree-sitter-go")
          (templ "https://github.com/vrischmann/tree-sitter-templ")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
