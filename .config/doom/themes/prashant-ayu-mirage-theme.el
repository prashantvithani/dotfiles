;;; doom-ayu-mirage-theme.el --- inspired by Ayu Mirage -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: February 5, 2021 (#499)
;; Author: LoveSponge <https://github.com/LoveSponge>
;; Maintainer:
;; Source: https://github.com/dempfi/ayu
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup prashant-ayu-mirage-theme nil
  "Options for the `prashant-ayu-mirage' theme."
  :group 'doom-themes)

(defcustom prashant-ayu-mirage-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'prashant-ayu-mirage-theme
  :type 'boolean)

(defcustom prashant-ayu-mirage-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'prashant-ayu-mirage-theme
  :type 'boolean)

(defcustom prashant-ayu-mirage-comment-bg prashant-ayu-mirage-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'prashant-ayu-mirage-theme
  :type 'boolean)

(defcustom prashant-ayu-mirage-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'prashant-ayu-mirage-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme prashant-ayu-mirage
    "A dark theme inspired by Ayu Mirage"
  :family 'prashant-ayu
  :background-mode 'dark

  ;; name        default   256       16
  (
   ;; common
   (common-accent   '("#ffcc66" "orange"  "orange" ))
   (common-error    '("#ff6666" "red"     "red"    ))
   ;; (common-fg       '("#cbccc6" "grey"    "grey"   ))
   (test            '("#7399e6" "grey"    "grey"   ))
   ;; syntax
   (syntax-tag      '("#5CCFE6" "cyan"    "blue"   ))
   (syntax-func     '("#FFD173" "yellow"  "yellow" ))
   (syntax-entity   '("#73D0FF" "blue"    "blue"   ))
   (syntax-string   '("#D5FF80" "green"   "green"  ))
   (syntax-regexp   '("#95E6CB" "teal"    "green"  ))
   (syntax-markup   '("#F28779" "red"     "red"    ))
   (syntax-keyword  '("#FFAD66" "orange"  "orange" ))
   (syntax-special  '("#FFDFB3" "yellow"  "yellow" ))
   (syntax-comment  (doom-lighten '("#B8CFE6" "grey"    "grey"   ) 80))
   (syntax-constant '("#DFBFFF" "magenta" "purple" ))
   (syntax-operator '("#f29e74" "orange"  "orange" ))
   ;; ui
   (ui-line               '("#171b24"))
   (ui-bg               '("#1f2430" "black"   "black"  ))
   (ui-fg               '("#707a8c" "grey"    "grey"   ))
   (ui-selection-normal   (doom-lighten '("#637599") 0.15))
   (ui-selection-active   (doom-lighten '("#69758c") 0.12))
   (ui-panel-bg           '("#1c212b"))
   (ui-panel-shadow       (doom-lighten '("#12151c") 0.7))
   (ui-gutter-normal      (doom-darken ui-fg 0.45))
   (ui-gutter-active      ui-fg)
   (ui-selection-border   (doom-lighten test 0.93))
   (ui-guide-normal       (doom-darken ui-fg 0.35))
   (ui-guide-active       (doom-darken ui-fg 0.75))
   (ui-org-block          (doom-darken ui-bg 0.10))
   (elscreen-bg           (doom-darken ui-fg 0.55))
   (elscreen-fg           ui-line)
   ;; vcs
   (vcs-added    '("#87d96c" "green" "green" ))
   (vcs-modified '("#80bfff" "blue"  "blue"  ))
   (vcs-removed  '("#f27983" "red"   "red"   ))
   ;; editor
   (editor-fg '("#cccac2"))
   (editor-bg '("#242936"))
   (editor-line '("#1A1F29"))

   (editor-find-match-active '("#695380"))
   (editor-find-match-inactive (doom-lighten '("#695380") 0.4))

   (editor-selection-active (doom-lighten '("#409FFF") 0.25))
   (editor-selection-inactive (doom-lighten '("#409FFF") 0.13))
   (editor-gutter-active (doom-lighten '("#8a9199") 0.8))
   (editor-gutter-normal (doom-lighten '("#8a9199") 0.4))
   (editor-indent-guide-active (doom-lighten '("#8a9199") 0.35))
   (editor-indent-guide-normal (doom-lighten '("#8a9199") 0.18))

   (bg         editor-bg)
   (bg-alt     editor-line)
   (base0      ui-bg)
   (base1      ui-panel-shadow)
   (base2      ui-selection-normal)
   (base3      ui-selection-active)
   (base4      editor-indent-guide-normal)
   (base5      editor-indent-guide-active)
   (base6      editor-gutter-normal)
   (base7      editor-gutter-active)
   (base8      editor-fg)
   (fg         editor-fg)
   (fg-alt     ui-fg)

   (grey       ui-line)
   (red        syntax-markup)
   (orange     syntax-keyword)
   (green      syntax-string)
   (teal       syntax-regexp)
   (yellow     syntax-func)
   (blue       syntax-entity)
   (dark-blue  (doom-darken syntax-entity 0.2))
   (magenta    syntax-constant)
   (violet     (doom-lighten syntax-constant 0.2))
   (cyan       syntax-tag)
   (dark-cyan  test)

   ;; face categories -- required for all themes
   (highlight      common-accent)
   (vertical-bar   ui-panel-bg)
   (selection      nil)
   (builtin        nil)
   (comments       (if prashant-ayu-mirage-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten comments 0.25))
   (constants      syntax-constant)
   (functions      syntax-func)
   (keywords       syntax-keyword)
   (methods        syntax-func)
   (operators      syntax-operator)
   (type           syntax-special)
   (strings        syntax-string)
   (variables      editor-fg)
   (numbers        syntax-func)
   (region         ui-selection-normal)
   (error          common-error)
   (warning        yellow)
   (success        green)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright prashant-ayu-mirage-brighter-modeline)
   (-modeline-pad
    (when prashant-ayu-mirage-padded-modeline
      (if (integerp prashant-ayu-mirage-padded-modeline) prashant-ayu-mirage-padded-modeline 4)))

   (modeline-fg     editor-fg)
   (modeline-fg-alt common-accent)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt))))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (diff-removed :foreground vcs-removed)
   (font-lock-comment-face
    :foreground comments
    :background (if prashant-ayu-mirage-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; company
   (company-tooltip :foreground editor-fg :background ui-bg)
   (company-tooltip-annotation :foreground editor-fg)
   (company-tooltip-selection :background ui-line)
   (company-tooltip-search :foreground common-accent :weight 'bold)
   (company-scrollbar-bg :background ui-bg)
   (company-scrollbar-fg :background syntax-comment)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg modeline-bg) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground green :weight 'normal)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background elscreen-bg :foreground elscreen-fg)
   ;;;; ivy
   (ivy-current-match :background ui-bg)
   (ivy-minibuffer-match-face-1 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground common-accent :weight 'bold)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten ui-bg 0.05))
   ;;;; org-mode
   (org-hide :foreground hidden)
   (org-headline-done :foreground syntax-comment)
   (org-document-info-keyword :foreground comments)
   (org-macro :foreground syntax-operator)
   ;;;; mic-paren
   ((paren-face-match &override) :foreground fg :background ui-selection-normal :weight 'ultra-bold)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-darken cyan 0.5))
   (rjsx-attr :foreground syntax-func)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground (doom-darken cyan 0.5))
   (web-mode-html-attr-name-face :foreground syntax-func)))

;;; prashant-ayu-mirage-theme.el ends here
