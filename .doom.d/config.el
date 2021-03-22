;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Prashant Vithani"
      user-mail-address "prashantvithani@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;;load custom functions file
(load "~/.doom.d/custom-functions")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; (global-auto-revert-mode t)

;;;; Local Leader
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;;;; Evil Snipe
(setq evil-snipe-override-evil-repeat-keys nil)
;; (evil-snipe-override-mode +1)
;; (when evil-snipe-override-evil-repeat-keys
;;   (evil-define-key 'motion evil-snipe-override-mode-map
;;     (kbd "C-;") 'evil-snipe-repeat
;;     (kbd "C-,") 'evil-snipe-repeat-reverse))

;;automatic indenting of pasted text (functions defined in custom-functions.el)
(map! :n "p" #'evil-paste-after-and-indent
      :n "P" #'evil-paste-before-and-indent)

;;;; Auto indent on paste
;; (defadvice! reindent-after-paste (&rest _)
;;   :after '(evil-paste-after evil-paste-before)
;;   (cl-destructuring-bind (_ _ _ beg end &optional _)
;;       evil-last-paste
;;     (evil-indent beg end)))

;;;; Custom Settings
(setq-default line-spacing 1
              read-quoted-char-radix 16)
(setq doom-font "Hermit-12:regular"
      ;; doom-big-font "Hermit-18:medium"
      doom-unicode-font "Noto Color Emoji-12:regular"
      doom-variable-pitch-font "Cantarell-12:medium"
      doom-serif-font "Iosevka Custom-12:regular"
      ;; mac-right-option-modifier nil
      ;; mac-command-modifier 'super
      projectile-project-search-path '("~/Workspace/repos")
      dired-dwim-target t
      +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir)
      magit-repository-directories '("~/Workspace/repos/")
      lsp-response-timeout 25
      lsp-enable-xref t
      +magit-hub-features t)
(after! git-gutter-fringe (fringe-mode '4))
(global-git-gutter-mode t)

(define-key key-translation-map [?\C-i]
  (λ! (if (and (not (cl-position 'tab    (this-single-command-raw-keys)))
               (not (cl-position 'kp-tab (this-single-command-raw-keys)))
               (display-graphic-p))
          [C-i] [?\C-i])))

;; (define-key doom-leader-map (kbd ":") 'projectile-find-file)
;; (define-key doom-leader-map (kbd "SPC") 'execute-extended-command)

;; Motion bindings
(map! :m [C-i] #'evil-jump-forward)

;; Normal-Emacs bindings
(map! :ne "M-/" #'comment-line)
(map! :ne "SPC s g" #'deadgrep)
(map! :ne "SPC n b" #'org-brain-visualize)
(map! :leader :desc "M-x" :ne "SPC" #'execute-extended-command)
(map! :leader :desc "Find file in project" :ne "." #'projectile-find-file)
(map! :leader :desc "Find file" :ne ":" #'find-file)

(map! :leader :ne "C-+" #'font-size-hidpi)

;; NOTE: Raise popup window - Info mode: C-~

;;;;;;;;;;;;;;;;; ORG ;;;;;;;;;;;;;;;;;;;;;;
;; (set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
;; (set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
;; (set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)

(advice-add #'+org|update-cookies :override #'+org*update-cookies)

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook! 'org-mode-hook (company-mode -1))
(add-hook! 'org-capture-mode-hook (company-mode -1))

(setq
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-ellipsis " ▾ "
 org-bullets-bullet-list '("·")
 org-archive-subtree-save-file-p t ; save target buffer after archiving
 org-tags-column -80
 org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t))
 org-log-done 'time
 org-refile-targets (quote ((nil :maxlevel . 1)))
 org-capture-templates '(("x" "Note" entry
                          (file+olp+datetree "journal.org")
                          "**** [ ] %U %?" :prepend t :kill-buffer t)
                         ("t" "Task" entry
                          (file+headline "tasks.org" "Inbox")
                          "* [ ] %?\n%i" :prepend t :kill-buffer t))
 +org-capture-todo-file "tasks.org")

;;;;;;;;;;;;;;;; SUPER AGENDA ;;;;;;;;;;;;;;;;
(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-include-diary t
        org-agenda-start-with-log-mode t
        org-agenda-compact-blocks t)
        ;; org-agenda-start-day nil
        ;; org-agenda-span 1
        ;; org-agenda-start-on-weekday nil)

  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day nil) ;; today
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)
                           (:discard (:anything))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "In Progress"
                             :todo ("STRT" "[-]")
                             :order 2)
                            (:name "Important"
                             :tag ("important" "imp")
                             :priority "A"
                             :order 2)
                            (:name "Due Today"
                             :deadline today
                             :order 3)
                            (:name "Due Soon"
                             :deadline future
                             :order 4)
                            (:name "Overdue"
                             :deadline past
                             :order 5)
                            (:name "Issues"
                             :tag "issue"
                             :order 6)
                            (:name "Reviews"
                             :tag "review"
                             :order 6)
                            (:name "Reviews"
                             :tag "review"
                             :order 6)
                            (:name "Meetings"
                             :tag "meeting"
                             :order 6)
                            (:name "Research"
                             :tag "research"
                             :order 8)
                            (:name "To read"
                             :tag "read"
                             :order 9)
                            (:name "Waiting"
                             :todo ("WAIT" "[?]")
                             :order 10)
                            (:name "trivial"
                             :priority<= "C"
                             :tag ("Trivial" "Unimportant")
                             :todo ("SOMEDAY" )
                             :order 90)
                            (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
  :config
  (org-super-agenda-mode))
;;;;;;;;;;;;;;;; SUPER AGENDA END ;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org
  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      :foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :foreground "#5B6268"
                      :background nil)
  (set-face-attribute 'org-level-1 nil
                      :foreground "steelblue2"
                      :background nil
                      :height 1.2
                      :weight 'normal)
  (set-face-attribute 'org-level-2 nil
                      :foreground "slategray2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-3 nil
                      :foreground "SkyBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-4 nil
                      :foreground "DodgerBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-5 nil
                      :weight 'normal)
  (set-face-attribute 'org-level-6 nil
                      :weight 'normal)
  (set-face-attribute 'org-document-title nil
                      :foreground "SlateGray1"
                      :background nil
                      :height 1.75
                      :weight 'bold)
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
;;;;;;;;;;;;;;;;; ORG END ;;;;;;;;;;;;;;;;;;;;;;

;;;; RUBY
(after! ruby
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                       ; Block end
                 ,(rx (or "#" "=begin"))                        ; Comment start
                 ruby-forward-sexp nil)))

(remove-hook 'ruby-mode-hook #'+ruby|init-robe)

;;;; Hide-Show
(add-to-list 'hs-special-modes-alist '(yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>" "" "#" +data-hideshow-forward-sexp nil))

;;; Indent guide hooks
(after! highlight-indent-guides
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column)
  ;; (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  (defadvice insert-for-yank (before my-clear-indent-guides activate)
    (remove-text-properties
     0 (length (ad-get-arg 0))
     '(display highlight-indent-guides-prop) (ad-get-arg 0))))


;;;; Dictionary
;; (user/dictionary)

;;;; Enable Fill column indicator for certain major modes
(user/fill-column-indicator-hooks)

;; (after! web-mode
;;   (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode)))

(add-hook 'Info-mode-hook
  (lambda()
    (evil-set-initial-state 'Info-mode 'emacs)
    (map! :ne "}" #'Info-scroll-up)
    (map! :ne "{" #'Info-scroll-down)))

;; ----- TRAMP on native-comp Emacs 28 -----
(after! tramp
  (unless (version<= emacs-version "28.0")
    (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
      "Start a program in a subprocess.  Return the process object for it.
 Similar to `start-process-shell-command', but calls `start-file-process'."
      ;; On remote hosts, the local `shell-file-name' might be useless.
      (let ((command (mapconcat 'identity args " ")))
        (funcall start-file-process-shell-command name buffer command)))
    (advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around))
  (setq tramp-default-method "ssh"
        tramp-use-ssh-controlmaster-options nil)
  (cl-pushnew 'tramp-own-remote-path tramp-remote-path)
  (require 'git-gutter-fringe))

;; ----- LSP over Tramp -----
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("solargraph" "stdio"))
                    :major-modes '(ruby-mode)
                    :remote? t
                    :server-id 'solargraph-remote)))

(ace-window-display-mode)

;; (after! Info-mode
;;   (evil-define-key 'motion 'Info-mode-map
;;     (kbd "C-n") 'Info-scroll-up
;;     (kbd "C-p") 'Info-scroll-down))

;;;; Help
;; Key bindings - Help navigation
;; (evil-define-key 'motion help-mode-map
;;   (kbd "C-n") 'Info-scroll-up
;;   (kbd "C-p") 'Info-scroll-down)

;; (def-package! parinfer ; to configure it
;;   :bind (("C-," . parinfer-toggle-mode)
;;          ("<tab>" . parinfer-smart-tab:dwim-right)
;;          ("S-<tab>" . parinfer-smart-tab:dwim-left))
;;   :hook ((clojure-mode emacs-lisp-mode common-lisp-mode lisp-mode) . parinfer-mode)
;;   :config (setq parinfer-extensions '(defaults pretty-parens evil paredit)))

;; (add-hook! reason-mode
;;   (add-hook 'before-save-hook #'refmt-before-save nil t))

;; (add-hook!
;;   js2-mode 'prettier-js-mode
;;   (add-hook 'before-save-hook #'refmt-before-save nil t))

;; dart-format-on-save t
;; web-mode-markup-indent-offset 2
;; web-mode-code-indent-offset 2
;; web-mode-css-indent-offset 2
;; js-indent-level 2
;; typescript-indent-level 2
;; prettier-js-args '("--single-quote")
;; json-reformat:indent-width 2
;; css-indent-offset 2

;; (use-package! lsp-ui
;;   :after (lsp-mode)
;;   :config
;;   (lsp-ui-doc-mode t))

;; (defun doom-project-ignored-p-override (project-root)
;;   (not (file-remote-p project-root)))
;; (advice-add 'doom-project-ignored-p :after-while #'doom-project-ignored-p-override)
