;;; prashant-kanagawa-theme.el --- inspired by rebelot/kanagawa.nvim and others -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 4 2023
;; Author: Anskrevy <https://github.com/Anskrevy
;; Maintainer:
;; Source: https://github.com/rebelot/kanagawa.nvim
;;
;;; Commentary:
;;; Original theme by rebelot see: https://github.com/rebelot/kanagawa.nvim
;;; Inspiration taken from modified version in https://github.com/NvChad/base46
;;; and konrad1977 https://github.com/konrad1977/emacs .
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup prashant-kanagawa-theme nil
  "Options for the `prashant-kanagawa' theme."
  :group 'doom-themes)

(defcustom prashant-kanagawa-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'prashant-kanagawa-theme
  :type 'boolean)

(defcustom prashant-kanagawa-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'prashant-kanagawa-theme
  :type 'boolean)

(defcustom prashant-kanagawa-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'prashant-kanagawa-theme
  :type '(choice integer boolean))

(defcustom prashant-kanagawa-red-cursor nil
  "If non-nil, cursor will be red."
  :group 'prashant-kanagawa-theme
  :type 'boolean)


;;
;;; Theme definition

(def-doom-theme prashant-kanagawa
  "A dark theme inspired by rebelot/kanagawa.nvim and others."

  ;; name        default   256           16
  ((bg         '("#181616" "black"       "black"  ))
   (fg         '("#DCD7BA" "#DCD7BA"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1D1C19" "black"       "black"        ))
   (fg-alt     '("#C8C093" "#C8C093"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#191922" "black"       "black"        ))
   (base1      '("#28282E" "#28282E"     "brightblack"  ))
   (base2      '("#222129" "#222129"     "brightblack"  ))
   (base3      '("#26252C" "#26252C"     "brightblack"  ))
   (base4      '("#37363A" "#37363A"     "brightblack"  ))
   (base5      '("#464546" "#464546"     "brightblack"  ))
   (base6      '("#545451" "#545451"     "brightblack"  ))
   (base7      '("#727169" "#727169"     "brightblack"  ))
   (base8      '("#BABDB9" "#BABDB9"     "white"        ))

   (comet          '("#54536D" "#54536D" "brightblue"   ))
   (spring-violet1 '("#938AA9" "#938AA9" "violet"       ))
   (spring-violet2 '("#9CABCA" "#717C7C" "violet"       ))
   (oni-violet     '("#957FB8" "#717C7C" "violet"       ))
   (crystal-blue   '("#7E9CD8" "#717C7C" "blue"         ))
   (spring-blue    '("#7FB4CA" "#717C7C" "blue"         ))
   (light-blue     '("#A3D4D5" "#717C7C" "brightblue"   ))
   (dragon-blue   '("#658594" "#658594" "blue"         ))
   (spring-green   '("#98BB6C" "#717C7C" "green"        ))
   (wave-red       '("#E46876" "#E46876" "red"          ))
   (sakura-pink    '("#D27E99" "#D27E99" "pink"         ))
   (boat-yellow1   '("#938056" "#938056" "brightyellow" ))
   (boat-yellow2   '("#C0A36E" "#C0A36E" "brightyellow" ))
   (carp-yellow    '("#E6C384" "#E6C384" "yellow"       ))
   (peach-red      '("#FF5D62" "#FF5D62" "red"          ))
   (surimi-orange  '("#FFA066" "#FFA066" "brightred"    ))
   (wave-blue-1    '("#223249" "#4e4e4e" "blue"         ))
   (wave-blue-2    '("#2D4F67" "#585858" "blue"         ))
   (wave-aqua-1    '("#6A9589" "#6a9589" "blue"         ))
   (wave-aqua-2    '("#7AA89F" "#717C7C" "blue"         ))
   (katana-gray    '("#717C7C" "#717C7C" "grey"         ))
   (fuji-gray      '("#727169" "#717C7C" "grey"         ))

   (samurai-red   '("#E82424" "#E82424" "red"          ))
   (ronin-yellow  '("#FF9E3B" "#FF9E3B" "yellow"       ))

   (autumn-yellow '("#DCA561" "#585858" "yellow"       ))
   (autumn-green  '("#76946A" "#76946A" "green"        ))
   (autumn-red    '("#C34043" "#C34043" "red"          ))

   (winter-green  '("#2B3328" "#585858" "green"        ))
   (winter-yellow '("#49443C" "#585858" "yellow"       ))
   (winter-red    '("#43242B" "#585858" "green"        ))
   (winter-blue   '("#252535" "#585858" "blue"         ))

   (sumi-ink0     '("#16161D" "#000000" "brightblack"  ))
   (sumi-ink1     '("#1F1F28" "#080808" "black"        ))
   (sumi-ink1b    '("#181820" "#000000" "black"        ))
   (sumi-ink2     '("#2A2A37" "#121212" "brightblack"  ))
   (sumi-ink3     '("#363646" "#303030" "black"        ))
   (sumi-ink4     '("#54546D" "#303030" "brightblack"  ))

   (grey          base7)
   (red           peach-red)
   (orange        surimi-orange)
   (green         spring-green)
   (teal          wave-aqua-2)
   (yellow        carp-yellow)
   (blue          spring-blue)
   (dark-blue     crystal-blue)
   (magenta       oni-violet)
   (violet        spring-violet2)
   (cyan          light-blue)
   (dark-cyan     dragon-blue)

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      comet)
   (vertical-bar   (doom-lighten base1 0.1))
   (selection      (doom-darken dark-blue 0.5))
   (builtin        blue)
   (comments       (if prashant-kanagawa-brighter-comments dark-cyan base7))
   (doc-comments   (doom-lighten (if prashant-kanagawa-brighter-comments dark-cyan comet) 0.25))
   (constants      yellow)
   (functions      dark-blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      boat-yellow2)
   (type           teal)
   (strings        green)
   (variables      wave-red)
   (numbers        sakura-pink)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          samurai-red)
   (warning        ronin-yellow)
   (success        teal)
   (vc-modified    blue)
   (vc-added       autumn-green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (info-xref                yellow)
   (modeline-fg-alt          base5)
   (modeline-bg              (if prashant-kanagawa-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if prashant-kanagawa-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when prashant-kanagawa-padded-modeline
      (if (integerp prashant-kanagawa-padded-modeline) prashant-kanagawa-padded-modeline 4))))


  ;;;; Base theme face overrides
  ((default :background sumi-ink1b :foreground fg)
   (cursor :background (if prashant-kanagawa-red-cursor red fg-alt))
   (highlight :background highlight :foreground spring-violet2 :distant-foreground base8)
   ((line-number &override) :foreground grey)
   ((line-number-current-line &override) :foreground fg)
   ;; ((font-lock-keyword-face &override) :foreground red)
   (font-lock-keyword-face :foreground magenta :weight 'semi-bold)
   (font-lock-negation-char-face :foreground red)
   ((font-lock-comment-face &override)
    :background (if prashant-kanagawa-brighter-comments (doom-lighten bg 0.05)))
   (elisp-shorthand-font-lock-face :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if prashant-kanagawa-brighter-modeline base8 highlight))
   (mode-line-highlight                           :foreground boat-yellow2)
   (mode-line-buffer-id                           :foreground blue :bold t)

   ;;;; indent-guides
   (highlight-indent-guides-character-face :foreground base4)
   (highlight-indent-guides-top-character-face :foreground base4)
   (highlight-indent-guides-stack-character-face :foreground base4)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground (if prashant-kanagawa-red-cursor red blue) :background bg-alt)
   ;;;; corfu
   (corfu-current :background base3)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; doom-modeline
   (doom-modeline-evil-motion-state               (:foreground cyan))
   (doom-modeline-evil-emacs-state                (:foreground dark-blue))
   (doom-modeline-evil-insert-state               (:foreground red))
   (doom-modeline-evil-normal-state               (:foreground cyan))
   (doom-modeline-evil-visual-state               (:foreground spring-green))
   (doom-modeline-evil-replace-state              (:foreground ronin-yellow))
   (doom-modeline-evil-operator-state             (:foreground dark-blue))

   (doom-modeline-project-dir                     :bold t :foreground blue)
   (doom-modeline-buffer-path                     :inherit 'bold :foreground blue)
   (doom-modeline-buffer-file                     :inherit 'bold :foreground magenta)
   (doom-modeline-buffer-modified                 :inherit 'bold :foreground yellow)
   (doom-modeline-error                           :background red)
   (doom-modeline-buffer-major-mode               :foreground blue :bold t)
   (doom-modeline-info                            :bold t :foreground cyan)
   (doom-modeline-project-dir                     :bold t :foreground orange)
   (doom-modeline-bar                             :bold t :background spring-violet2)
   (doom-modeline-panel                           :inherit 'bold :background boat-yellow2 :foreground sumi-ink2)
   (doom-themes-visual-bell                       :background autumn-red)
   ;; (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   ;;;; vterm
   (vterm-color-bright-black :inherit 'term-color-bright-black :foreground base5))

  ;;;; Base theme variable overrides-
  ())

;;; prashant-kanagawa-theme.el ends here
