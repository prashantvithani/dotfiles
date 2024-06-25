;;; tree-sitter-configs.el --- Tree Sitter Language configs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Prashant Vithani

;; Author: Prashant Vithani <prashantvithani@gmail.com>
;; Keywords: languages

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (common-lisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
          (emacs-lisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/serenadeai/java-tree-sitter"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
          (ruby . ("https://github.com/calicoday/ruby-tree-sitter-ffi"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
          (scheme . ("https://github.com/6cdh/tree-sitter-scheme"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (ssh . ("https://github.com/metio/tree-sitter-ssh-client-config"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :config
  (defun user/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (go-mode . go-ts-mode))))
