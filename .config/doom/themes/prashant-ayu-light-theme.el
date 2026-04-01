;;; prashant-ayu-light-theme.el --- inspired by Ayu Dark -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defgroup prashant-ayu-light-theme nil
  "Options for the `prashant-ayu-light' theme."
  :group 'doom-themes)

(defcustom prashant-ayu-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'prashant-ayu-light-theme
  :type 'boolean)

(defcustom prashant-ayu-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'prashant-ayu-light-theme
  :type 'boolean)

(defcustom prashant-ayu-light-comment-bg prashant-ayu-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'prashant-ayu-light-theme
  :type 'boolean)

(defcustom prashant-ayu-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'prashant-ayu-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme prashant-ayu-light
    "A dark theme inspired by Ayu Dark"
  :family 'prashant-ayu
  :background-mode 'light

  ;; name        default   256       16
  (
   ;; common
   (common-accent-tint   '("#f29718" "yellow"  "yellow" ))
   (common-accent-on  (doom-darken common-accent-tint 0.31))
   (common-error    '("#e65050" "red"     "red"    ))
   (common-fg       '("#cbccc6" "grey"    "grey"   ))
   (test            '("#7399e6" "grey"    "grey"   ))
   ;; syntax
   (syntax-tag      '("#55b4d4" "cyan"    "blue"   ))
   (syntax-func     '("#eba400" "yellow"  "yellow" ))
   (syntax-entity   '("#22a4e6" "blue"    "blue"   ))
   (syntax-string   '("#86b300" "green"   "green"  ))
   (syntax-regexp   '("#4cbf99" "teal"    "green"  ))
   (syntax-markup   '("#f07171" "red"     "red"    ))
   (syntax-keyword  '("#fa8532" "orange"  "orange" ))
   (syntax-special  '("#e59645" "yellow"  "yellow" ))
   (syntax-comment  '("#adaeb1" "grey"    "grey"   )) ;; (doom-lighten '("#B8CFE6" "grey"    "grey"   ) 80)
   (syntax-constant '("#a37acc" "magenta" "purple" ))
   (syntax-operator '("#f2a191" "pink"  "pink" ))
   ;; ui
   (ui-line               (doom-lighten '("#687d8f") 0.12))
   (ui-bg               '("#f8f9fa" "black"   "black"  ))
   (ui-fg               '("#828e9f" "grey"    "grey"   ))
   (ui-selection-active   (doom-lighten ui-line 0.14))
   (ui-selection-normal   ui-line)
   (ui-panel-bg           '("#fafafa"))
   (ui-panel-shadow       (doom-lighten ui-line 0.07))
   (ui-gutter-normal      (doom-darken ui-fg 0.45))
   (ui-gutter-active      ui-fg)
   (ui-selection-border   (doom-lighten test 0.93))
   (ui-guide-normal       (doom-darken ui-fg 0.35))
   (ui-guide-active       (doom-darken ui-fg 0.75))
   (ui-org-block          (doom-darken ui-bg 0.10))
   (elscreen-bg           (doom-darken ui-fg 0.55))
   (elscreen-fg           ui-line)
   ;; vcs
   (vcs-added    '("#6cbf43" "green" "green" ))
   (vcs-modified '("#478acc" "blue"  "blue"  ))
   (vcs-removed  '("#ff7383" "red"   "red"   ))
   ;; editor
   (editor-fg '("#5c6166"))
   (editor-bg '("#fcfcfc"))
   (editor-line (doom-lighten ui-fg 0.1))

   ;; (editor-find-match-active '("#695380"))
   ;; (editor-find-match-inactive (doom-lighten '("#695380") 0.4))

   (editor-selection-active (doom-lighten '("#035BD6") 0.15))
   (editor-selection-inactive (doom-lighten editor-selection-active 0.07))
   (editor-find-match-active "#ffe294")
   (editor-find-match-inactive (doom-lighten editor-find-match-active 0.5))
   (editor-line-number-active (doom-darken ui-fg 0.8))
   (editor-line-number-inactive (doom-darken ui-fg 0.18))
   (editor-gutter-active (doom-lighten '("#8a9199") 0.6))
   (editor-gutter-normal (doom-lighten '("#8a9199") 0.5))
   (editor-indent-guide-active (doom-darken ui-fg 0.35))
   (editor-indent-guide-normal (doom-darken ui-fg 0.18))

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
   (violet     (doom-blend magenta blue 0.7))
   (cyan       syntax-tag)
   (dark-cyan  (doom-darken cyan 0.3))

   ;; face categories -- required for all themes
   (highlight      editor-find-match-inactive)
   (vertical-bar   ui-panel-shadow)
   (selection      editor-selection-active)
   (builtin        syntax-tag)
   (comments       (if prashant-ayu-light-brighter-comments dark-cyan syntax-comment))
   (doc-comments   (doom-lighten comments 0.25))
   (constants      syntax-constant)
   (functions      syntax-func)
   (keywords       syntax-keyword)
   (methods        syntax-func)
   (operators      syntax-operator)
   (type           syntax-entity)
   (strings        syntax-string)
   (variables      (doom-lighten syntax-special 0.25))
   (numbers        syntax-markup)
   (region         ui-selection-active)
   (error          common-error)
   (warning        yellow)
   (success        green)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright prashant-ayu-light-brighter-modeline)
   (-modeline-pad
    (when prashant-ayu-light-padded-modeline
      (if (integerp prashant-ayu-light-padded-modeline) prashant-ayu-light-padded-modeline 4)))

   (modeline-fg     editor-fg)
   (modeline-fg-alt common-accent-tint)

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

  ;; Base theme face overrides
  (((line-number &override) :foreground editor-line-number-inactive)
   ((line-number-current-line &override) :foreground editor-line-number-active)
   (diff-removed :foreground vcs-removed)
   (hl-line :background bg-alt)

   ;; Font lock
   ((font-lock-comment-face &override) :foreground comments :slant 'italic
    :background (if prashant-ayu-light-comment-bg (doom-lighten bg 0.05) 'unspecified))
   ((font-lock-comment-delimiter-face &override) :foreground comments :italic t)
   ((font-lock-doc-face &override) :inherit 'font-lock-comment-face :foreground doc-comments)
   ((font-lock-constant-face &override) :foreground constants :weight 'semi-bold)
   ((font-lock-keyword-face &override) :foreground keywords :weight 'bold :slant 'italic)
   ((font-lock-number-face &override) :foreground numbers :weight 'bold)
   ((font-lock-function-name-face &override) :foreground functions :weight 'medium)
   ((font-lock-type-face &override) :foreground type :weight 'medium)
   ((font-lock-builtin-face &override) :foreground builtin :weight 'bold)
   ((font-lock-warning-face &override) :foreground warning)
   ((font-lock-variable-use-face &override) :foreground (doom-lighten syntax-regexp 0.3))
   ((font-lock-negation-char-face &override) :foreground red)
   ((font-lock-regexp-face &override) :foreground syntax-regexp)
   ((font-lock-preprocessor-face &override) :foreground (doom-lighten operators 0.5))
   ;; ((font-lock-regexp-grouping-backslash &override) :foreground operators)
   ;; ((font-lock-string-face &override) :foreground strings)
   ;; ((font-lock-variable-name-face &override) :foreground variables)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;; company
   (company-tooltip :foreground editor-fg :background ui-bg)
   (company-tooltip-annotation :foreground editor-fg)
   (company-tooltip-selection :background ui-line)
   (company-tooltip-search :foreground common-accent-tint :weight 'bold)
   (company-scrollbar-bg :background ui-bg)
   (company-scrollbar-fg :background syntax-comment)

   ;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg modeline-bg) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :foreground magenta :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold :foreground teal)
   (doom-modeline-buffer-major-mode :foreground teal :bold t)
   (doom-modeline-buffer-modified :foreground yellow :bold t)

   ;; Doom Dashboard
   (doom-dashboard-banner :foreground comments :slant 'normal)
   (doom-dashboard-loaded :foreground comments :slant 'normal)
   (doom-dashboard-menu-title :foreground keywords :weight 'semi-bold :slant 'normal)

   ;; elscreen
   (elscreen-tab-other-screen-face :background elscreen-bg :foreground elscreen-fg)

   ;; ivy
   (ivy-current-match :background ui-bg)
   (ivy-minibuffer-match-face-1 :foreground common-accent-tint :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground common-accent-tint :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground common-accent-tint :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground common-accent-tint :weight 'bold)

   ;; vertico
   ((vertico-current &override) :foreground yellow :bold t)
   (vertico-posframe-border :background base4)
   (vertico-posframe :background base3)

   ;;; Treemacs
   (treemacs-directory-face :foreground teal :weight 'semi-bold)

   ;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten ui-bg 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-headline-done :foreground syntax-comment)
   (org-document-info-keyword :foreground comments)
   (org-macro :foreground syntax-operator)
   (org-block :background base0 :foreground base7)
   (org-block-begin-line
    :background (doom-darken teal 0.8)
    :foreground teal)
   (org-block-end-line
    :background (doom-darken red 0.8)
    :foreground red)

   ;; mic-paren
   ((paren-face-match &override) :foreground fg :background ui-selection-normal :weight 'ultra-bold)

   ;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-darken cyan 0.5))
   (rjsx-attr :foreground syntax-func)

   ;; solaire-mode
   (solaire-hl-line-face :background base5)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; magit
   (magit-diff-removed                :foreground (doom-darken vc-deleted 0.2) :background (doom-blend vc-deleted bg 0.1) :extend t)
   (magit-diff-removed-highlight      :foreground vc-deleted                   :background (doom-blend vc-deleted bg 0.2) :weight 'bold :extend t)
   (magit-diff-hunk-heading           :foreground fg                    :background (doom-blend violet bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground bg                   :background violet :weight 'bold :extend t)

    ;;;; whitespace <built-in>
   (whitespace-tab
    :foreground base5
    :background (if (default-value 'indent-tabs-mode) 'unspecified base4))

   ;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground (doom-darken cyan 0.5))
   (web-mode-html-attr-name-face :foreground syntax-func)))


;;; prashant-ayu-light-theme.el ends here
