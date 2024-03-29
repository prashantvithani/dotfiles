;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
    '(
       ;; ----------------------------------------------------------------
       ;; Example of useful layers you may want to use right away.
       ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
       ;; `M-m f e R' (Emacs style) to install them.
       ;; ----------------------------------------------------------------
       (auto-completion :variables
         auto-completion-use-company-box t
         auto-completion-enable-help-tooltip t
         auto-completion-enable-snippets-in-popup t)
       better-defaults
       git
       github
       helm
       (lsp :variables
         lsp-ui-sideline-enable nil
         lsp-enable-completion-at-point nil)
       markdown
       multiple-cursors
       (org :variables
         org-enable-github-support t
         org-projectile-file "TODOs.org")
       (shell :variables
         shell-default-height 30
         shell-default-position 'bottom)
       (spell-checking :variables
         spell-checking-enable-by-default nil)
       syntax-checking
       treemacs
       version-control
       (osx :variables
         osx-use-dictionary-app nil)
       (ruby :variables
         ruby-version-manager 'rvm
         ruby-test-runner 'rspec
         ruby-backend 'lsp
         ruby-insert-encoding-magic-comment nil)
       ruby-on-rails
       common-lisp
       emacs-lisp
       scheme
       sql
       python
       (c-c++ :variables c-c++-backend 'lsp-clangd)
       java
       (go :variables
         go-tab-width 4
         go-format-before-save t
         gofmt-command "goimports"
         go-use-golangci-lint t
         godoc-at-point-function 'godoc-gogetdoc)
       html
       yaml
       dash
       docker
       protobuf
       themes-megapack
       )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(dictionary evil-snipe highlight-indent-guides)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
     [default bold shadow italic underline bold bold-italic bold])
  '(ansi-color-names-vector
     ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#93E0E3")
 '(cua-normal-cursor-color "#DCDCCC")
 '(cua-overwrite-cursor-color "#F0DFAF")
 '(cua-read-only-cursor-color "#7F9F7F")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#6272a4")
 '(highlight-changes-colors '("#DC8CC3" "#bbb0cb"))
  '(highlight-symbol-colors
     '("#680f63eb5998" "#54db645064d0" "#6097535f5322" "#5c2859a95fa1" "#4ede55f24ea4" "#64dd5979525e" "#530060d16157"))
 '(highlight-symbol-foreground-color "#FFFFEF")
  '(highlight-tail-colors
     '(("#4F4F4F" . 0)
        ("#488249" . 20)
        ("#5dacaf" . 30)
        ("#57a2a4" . 50)
        ("#b6a576" . 60)
        ("#ac7b5a" . 70)
        ("#aa5790" . 85)
        ("#4F4F4F" . 100)))
  '(hl-bg-colors
     '("#b6a576" "#ac7b5a" "#9f5c5c" "#aa5790" "#85749c" "#57a2a4" "#5dacaf" "#488249"))
  '(hl-fg-colors
     '("#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F"))
 '(hl-sexp-background-color "#1c1f26")
  '(hl-todo-keyword-faces
     '(("TODO" . "#dc752f")
        ("NEXT" . "#dc752f")
        ("THEM" . "#2d9574")
        ("PROG" . "#4f97d7")
        ("OKAY" . "#4f97d7")
        ("DONT" . "#f2241f")
        ("FAIL" . "#f2241f")
        ("DONE" . "#86dc2f")
        ("NOTE" . "#b1951d")
        ("KLUDGE" . "#b1951d")
        ("HACK" . "#b1951d")
        ("TEMP" . "#b1951d")
        ("FIXME" . "#dc752f")
        ("XXX+" . "#dc752f")
        ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(lsp-ui-doc-border "#FFFFEF")
  '(nrepl-message-colors
     '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#ff5555")
  '(package-selected-packages
     '(company-quickhelp company-box frame-local shrink-path ox-gfm protobuf-mode helm helm-core highlight-indent-guides tide typescript-mode import-js grizzl add-node-modules-path evil-snipe doom-modeline helm-rtags google-c-style flycheck-ycmd flycheck-rtags disaster cquery cpp-auto-include company-ycmd ycmd request-deferred company-rtags rtags company-c-headers clang-format ccls dockerfile-mode docker tablist docker-tramp material-theme zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme eziam-theme exotica-theme espresso-theme dracula-theme doom-themes django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme chocolate-theme autothemer cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme utop tuareg caml ocp-indent ob-elixir nodejs-repl mvn meghanada maven-test-mode lsp-java livid-mode skewer-mode json-navigator hierarchy json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc groovy-mode groovy-imports gradle-mode flycheck-ocaml merlin flycheck-mix flycheck-credo emojify emoji-cheat-sheet-plus dune company-tern tern company-emoji auto-complete-rst alchemist elixir-mode geiser link connection dictionary helm-dash dash-docs dash-at-point yapfify stickyfunc-enhance pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements lsp-python-ms live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-gtags helm-cscope xcscope ggtags cython-mode counsel-gtags counsel swiper ivy company-anaconda blacken anaconda-mode pythonic flycheck-golangci-lint sqlup-mode sql-indent web-mode web-beautify tagedit slim-mode scss-mode sass-mode pug-mode prettier-js impatient-mode simple-httpd helm-css-scss haml-mode emmet-mode company-web web-completion-data godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc company-go go-mode slime-company slime common-lisp-snippets orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink org-brain org-ql peg ov org-super-agenda ts htmlize helm-org-rifle helm-org gnuplot github-search github-clone gist gh marshal logito pcache forge ghub closql emacsql-sqlite emacsql treepy evil-org xterm-color vterm terminal-here shell-pop multi-term eshell-z eshell-prompt-extras esh-help unfill mwim dap-mode bui enh-ruby-mode yasnippet-snippets yaml-mode treemacs-magit smeargle seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv projectile-rails rake inflections mmm-mode minitest markdown-toc magit-svn magit-section magit-gitflow magit-popup lsp-ui lsp-treemacs helm-lsp helm-gitignore helm-git-grep helm-company helm-c-yasnippet gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ fringe-helper git-gutter+ gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip feature-mode evil-magit magit git-commit with-editor transient company-lsp lsp-mode markdown-mode dash-functional company chruby bundler inf-ruby browse-at-remote auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete reveal-in-osx-finder osx-trash osx-dictionary osx-clipboard launchctl ws-butler writeroom-mode winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-persp treemacs-evil toc-org symon symbol-overlay string-inflection spaceline-all-the-icons restart-emacs request rainbow-delimiters popwin pcre2el password-generator paradox overseer org-plus-contrib org-bullets open-junk-file nameless move-text macrostep lorem-ipsum link-hint indent-guide hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio font-lock+ flycheck-package flycheck-elsa flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu elisp-slime-nav editorconfig dumb-jump dotenv-mode diminish devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
 '(pos-tip-background-color "#4F4F4F")
 '(pos-tip-foreground-color "#FFFFEF")
  '(rustic-ansi-faces
     ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
  '(safe-local-variable-values
     '((whitespace-line-column . 80)
        (ruby-test-runner quote minitest)
        (ruby-test-runner . minitest)
        (go-backend . go-mode)
        (go-backend . lsp)))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#7F9F7F" "#4F4F4F" 0.2))
 '(term-default-bg-color "#3F3F3F")
 '(term-default-fg-color "#DCDCCC")
 '(vc-annotate-background "#282a36")
 '(vc-annotate-background-mode nil)
  '(vc-annotate-color-map
     (list
       (cons 20 "#50fa7b")
       (cons 40 "#85fa80")
       (cons 60 "#bbf986")
       (cons 80 "#f1fa8c")
       (cons 100 "#f5e381")
       (cons 120 "#face76")
       (cons 140 "#ffb86c")
       (cons 160 "#ffa38a")
       (cons 180 "#ff8ea8")
       (cons 200 "#ff79c6")
       (cons 220 "#ff6da0")
       (cons 240 "#ff617a")
       (cons 260 "#ff5555")
       (cons 280 "#d45558")
       (cons 300 "#aa565a")
       (cons 320 "#80565d")
       (cons 340 "#6272a4")
       (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil)
  '(weechat-color-list
     '(unspecified "#3F3F3F" "#4F4F4F" "#9f5c5c" "#CC9393" "#488249" "#7F9F7F" "#b6a576" "#F0DFAF" "#57a2a4" "#8CD0D3" "#aa5790" "#DC8CC3" "#5dacaf" "#93E0E3" "#DCDCCC" "#6F6F6F"))
  '(xterm-color-names
     ["#4F4F4F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#fffff6"])
  '(xterm-color-names-bright
     ["#3F3F3F" "#DFAF8F" "#878777" "#6F6F6F" "#DCDCCC" "#bbb0cb" "#FFFFEF" "#FFFFFD"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                                :size 10.0
                                :weight normal
                                :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq
    mac-right-option-modifier nil
    dotspacemacs-default-font '("Iosevka Custom"
                                 :size 12.0
                                 :weight normal
                                 :width normal
                                 :powerline-scale 1.5)
    dotspacemacs-line-numbers '(:relative t
                                 :disabled-for-modes
                                 dired-mode
                                 doc-view-mode
                                 markdown-mode
                                 org-mode
                                 pdf-view-mode
                                 text-mode
                                 )
    dotspacemacs-whitespace-cleanup 'changed
    dotspacemacs-maximized-at-startup t
    dotspacemacs-mode-line-theme '(all-the-icons :separator-scale 1.5)
    )
  (setq org-agenda-files '("~/org"))

  ;; Flycheck hooks
  ;; (user/flycheck-hooks)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  (setq evil-snipe-auto-disable-substitute nil)
  (require 'evil-snipe)
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (global-company-mode)
  (setq-default
    line-spacing 1
    ruby-indent-tabs-mode nil
    )

  ;;;; Custom settings
  (setq
    lsp-response-timeout 25
    lsp-enable-xref t
    magit-repository-directories '("~/Workspace/repos/")
    dotspacemacs-distinguish-gui-tab t
    )

  ;;;; Enh Ruby
  ;; indentation for enh-ruby
  (setq
    enh-ruby-deep-indent-paren t
    enh-ruby-hanging-paren-deep-indent-level 0
    enh-ruby-hanging-brace-deep-indent-level 1
    )

  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
       `(ruby-mode
          ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
          ,(rx (or "}" "]" "end"))                       ; Block end
          ,(rx (or "#" "=begin"))                        ; Comment start
          ruby-forward-sexp nil)))

  ;;;; Info
  ;; Key bindings - Info navigation
  (evil-define-key 'motion Info-mode-map
    (kbd "C-n") 'Info-scroll-up
    (kbd "C-p") 'Info-scroll-down)

  ;;;; Help
  ;; Key bindings - Help navigation
  (evil-define-key 'motion help-mode-map
    (kbd "C-n") 'Info-scroll-up
    (kbd "C-p") 'Info-scroll-down)

  ;; Tab hooks
  (user/tab-hooks)

  ;; FCI-mode hooks
  (user/fill-column-indicator-hooks)

  ;; Indent highlight hooks
  (user/highlight-indent-guide-hooks)

  ;;;; Dicitonary
  (user/dictionary)

  ;; Evil-Snipe
  (evil-snipe-override-mode +1)
  (when evil-snipe-override-evil-repeat-keys
    (evil-define-key 'motion evil-snipe-override-mode-map
      (kbd "C-;") 'evil-snipe-repeat
      (kbd "C-,") 'evil-snipe-repeat-reverse))
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'visible)
  (setq evil-snipe-spillover-scope 'buffer)
  (setq evil-snipe-auto-disable-substitute nil)

  ;; Spaceline
  (setq spaceline-all-the-icons-separator-type 'arrow)
  (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)

  ;; Variable Pitch fonts
  (set-face-attribute 'variable-pitch nil :font "Avenir Next")

  ;; Remove duplicates from helm commands history
  (setq history-delete-duplicates t)

  ;; Enable projectile caching
  (setq projectile-enable-caching t)

  ;; ORG mode settings
  (add-hook 'org-mode-hook #'(lambda ()
                               ;; Wrap lines by edge of screen
                               (visual-line-mode)
                               (org-indent-mode)))
  (setq org-todo-keywords
    '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "IN-PROGRESS(p)" "IN-REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)" "DELEGATED(e)")))
  (setq org-archive-subtree-save-file-p t)
  )

(defun user/tab-hooks ()
  ;; Tab setting hooks
  (add-hook 'ruby-mode-hook 'user/tab-settings)
  (add-hook 'python-mode-hook 'user/tab-settings)
  (add-hook 'yaml-mode-hook 'user/tab-settings)
  (add-hook 'sql-mode-hook 'user/tab-settings)
  (add-hook 'conf-mode-hook 'user/tab-settings)
  (add-hook 'emacs-lisp-mode-hook 'user/tab-settings)
  (add-hook 'common-lisp-mode-hook 'user/tab-settings)
  (add-hook 'scheme-mode-hook 'user/tab-settings)
  (add-hook 'c-c++-mode-hook 'user/tab-settings)
  (add-hook 'c++-mode-hook 'user/tab-settings)
  (add-hook 'c-mode-hook 'user/tab-settings)
  )

;;;; Tab and indentation settings
(defun user/tab-settings ()
  (set (make-local-variable 'indent-tabs-mode) 'nil)
  (set (make-local-variable 'tab-width) 2))

;;;; Enable Fill column indicator for certain major modes
(defun user/fill-column-indicator-hooks ()
  (add-hook 'ruby-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'c-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'java-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'python-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'go-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'common-lisp-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'scheme-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  )

;;;; Customize flycheck for different modes
(defun user/flycheck-hooks ()
  (add-hook 'ruby-mode-hook (lambda () (setq flycheck-checker 'ruby-rubocop)))
  )

(defun user/highlight-indent-guide-hooks ()
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (defadvice insert-for-yank (before my-clear-indent-guides activate)
    (remove-text-properties
      0 (length (ad-get-arg 0))
      '(display highlight-indent-guides-prop) (ad-get-arg 0)))
  )

;;;; Dictionary
(defun user/dictionary ()
  "All settings related to dictionary"
  ;;;; Dictionary
  (require 'dictionary)

  ;; Customize
  (setq dictionary-default-dictionary "wn")

  ;; key bindings - history go back
  (evil-define-key 'motion dictionary-mode-map
    (kbd "C-o") 'dictionary-previous)

  ;; Invoke
  (spacemacs/set-leader-keys "xwf" 'dictionary-lookup-definition)
  (spacemacs/set-leader-keys "xwD" 'dictionary-search)

  ;; Font face -- make dictionary beautiful
  (set-face-font 'dictionary-word-definition-face "Cascadia Code")
  (set-face-attribute 'dictionary-button-face nil :foreground "magenta")
  (set-face-attribute 'dictionary-reference-face nil :foreground "cyan")
  (set-face-attribute 'dictionary-word-entry-face nil :foreground "yellow")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
