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
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     emacs-lisp
     git
     helm
     ;; lsp
     markdown
     ;; multiple-cursors
     (org :variables
          org-startup-truncated nil
          org-enable-roam-support t
          org-enable-roam-ui t)
     spell-checking
     themes-megapack
     treemacs)


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

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

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
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
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
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

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-one-light)

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

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Roboto Mono"
                               :size 13.0
                               :weight light
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

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
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

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

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Add pdflatex to the PATH for exporting reports
  (getenv "PATH")
  (setenv "PATH" (concat "/Library/TeX/texbin/" ":" (getenv "PATH")))

  ;; Don't ask for confirmation opening symlinks
  (setq vc-follow-symlinks t)

  ;; ------------------------------------------
  ;; ---------- 1. UI CUSTOMISATIONS ----------
  ;; ------------------------------------------

  ;; Add confirmation for quitting emacs
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Set global default directory
  (setq default-directory "~/emacs/")

  ;; -----------------------------------
  ;; ---------- 2. ORG SET UP ----------
  ;; -----------------------------------
  (with-eval-after-load 'org
    ;; --------------------------------------------
    ;; ---------- 2.1. DEFAULT DIRECTORIES ----------
    ;;---------------------------------------------
    ;; Set default directories
    (setq org-default-notes-file "~/emacs/org/inbox.org")
    (setq org-roam-directory "~/emacs/org/roam/")

    ;; Add all org files to org-agenda
    (setq org-agenda-files (directory-files-recursively"~/emacs/org/" "\\.org$"))

    ;; -----------------------------------------
    ;; ---------- 2.2 ORG-ROAM SET UP ----------
    ;; -----------------------------------------
    (use-package org-roam
      :ensure t
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-completion-everywhere t)
      :bind (("C-c n i" . org-roam-node-insert-immediate))
      :config
      (org-roam-db-autosync-mode))

    (use-package consult-org-roam
      :ensure t
      :after org-roam
      :init
      (require 'consult-org-roam)
      ;; Activate the minor mode
      (consult-org-roam-mode 1)
      :custom
      ;; Use `ripgrep' for searching with `consult-org-roam-search'
      (consult-org-roam-grep-func #'consult-ripgrep)
      ;; Configure a custom narrow key for `consult-buffer'
      (consult-org-roam-buffer-narrow-key ?r)
      ;; Display org-roam buffers right after non-org-roam buffers
      ;; in consult-buffer (and not down at the bottom)
      (consult-org-roam-buffer-after-buffers t)
      :config
      ;; Eventually suppress previewing for certain functions
      (consult-customize
       consult-org-roam-forward-links
       :preview-key "M-.")
      :bind
      ;; Define some convenient keybindings as an addition
      ("C-c n b" . consult-org-roam-backlinks)
      ("C-c n B" . consult-org-roam-backlinks-recursive)
      ("C-c n l" . consult-org-roam-forward-links))

    ;; Follow the links
    (setq org-return-follows-link  t)

    ;; Make the indentation look nicer
    (add-hook 'org-mode-hook 'org-indent-mode)

    ;; Insert immediate function to replace immediate
    (defun org-roam-node-insert-immediate (arg &rest args)
      (interactive "P")
      (let ((args (cons arg args))
            (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                      '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

    ;; --------------------------------------------------------
    ;; ---------- 2.3 ORG-AGENDA TODO CUSTOMISATIONS ----------
    ;; --------------------------------------------------------

    (setq org-agenda-skip-unavailable-files t) ;; Auto skip unavailable files

    ;; TODO states
    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "|" "DONE(d!)" )
            ))

    ;; TODO colors
    (setq org-todo-keyword-faces
          '(
            ("TODO" . (:foreground "GoldenRod" :weight bold))
            ("IN-PROGRESS" . (:foreground "LightBlue" :weight bold))
            ("DONE" . (:foreground "LimeGreen" :weight bold))
            ))


    ;; When a TODO is set to a done state, record a timestamp
    (setq org-log-done 'time)


    ;; -----------------------------------------------
    ;; ---------- 2.4 ORG-CAPTURE TEMPLATES ----------
    ;; -----------------------------------------------

    ;; Empty capture template list to stop duplication on eval-buffer
    (setq org-capture-templates nil)

    ;; Set capture template for tasks
    (add-to-list 'org-capture-templates
                 '("t" "Task" entry
                   (file+headline "/Users/djs30/emacs/org/tasks.org" "Tasks")
                   "* TODO [#B] %?\n:CREATED: %T\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n ":empty-lines 0))

    ;; Set capture template for meeting notes
    (add-to-list 'org-capture-templates
                 '("m" "Meeting notes"
                   entry (file buffer-name)
                   "* %? :meeting:\n:CREATED: %T\n** Attendees\n- \n** Preparation\n- \n** Discussion Points\n- \n** Action Items\n "
                   :tree-type week
                   :clock-in t
                   :clock-resume t
                   :empty-lines 0)
                 )

    ;; Set capture template for 1-1s
    (add-to-list 'org-capture-templates
                 '("o" "1-1" entry
                   (file+olp+datetree buffer-name)
                   (file "~/emacs/capture_templates/1-1.org")))

    ;; Set capture template for new project
    (add-to-list 'org-capture-templates
                 '("p" "Project template" entry
                   (file buffer-name)
                   (file "~/emacs/capture_templates/project.org")))

    ;; Set capture template for a person
    (add-to-list 'org-capture-templates
                 '("z" "Person" entry
                   (file buffer-name)
                   (file "~/emacs/capture_templates/person.org")))

    ;; Set capture template for the Weekly 5-15
    (add-to-list 'org-capture-templates
                 '("w" "5-15" entry
                   (file+olp+datetree "/Users/djs30/emacs/org/5-15.org")
                   (file "~/emacs/capture_templates/5-15_TP.org")))
    )

  ;; Set capture template for todo
  (add-to-list 'org-capture-templates
               '("t" "TODO" entry
                 (file buffer-name)
                 "*** TODO [#B] %?\n:CREATED: %T\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n ":empty-lines 0))
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
   '(org-agenda-files
     '("/Users/djs30/emacs/org/Import/Archive/FluidCloud/Team/James Forbes.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Team/Luca Nunzi.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Team/Martin Begley.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Team/Matthew Beckett.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Team/Prasanth Anandan.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Team/Rama Allamraju.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Ways of Working/How We Plan, Socialise, and Visualise Our Work.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Ways of Working/Replenishment & Weekly Checkout.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Ways of Working/Sit Down (Stand Up).md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/Ways of Working/Weekly Delivery Planning.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud/FluidCloud Opex.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/230424.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/230501.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/CIEC Macbooks.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/Decom - Paul Shires.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/IMP DNE.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/TSA & New Hires - Craig.md.org"
       "/Users/djs30/emacs/org/Import/Archive/LSD Decommission/Timesheet Adjustments.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Operational Efficiencies/Acceptance Criteria.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Operational Efficiencies/Operational Efficiencies.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240321 QuickNote - Private Cloud Savings.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240326 QuickNote - Introducing Velero Container Snapshots.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240516 QuickNote - Travel.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240604 QuickNote - Renato 5-15 questions.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240605 QuickNote - Portugal Travel.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240606 Telco Overview.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240620 Opex Capex.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240625 CIEC - Bi-weekly.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240702 Capacity & Distribution Tracking.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240702 QuickNote - Handover.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240703 QuickNote - TODO.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240730 QuickNote - Information Strategy.md.org"
       "/Users/djs30/emacs/org/Import/Archive/240801 QuickNote - Transformation Strategy.md.org"
       "/Users/djs30/emacs/org/Import/Archive/CKE CKS Discussion.md.org"
       "/Users/djs30/emacs/org/Import/Archive/CTF Template Stage.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Comcast Demo.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Davide_Workshop.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud Customers.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud Presentation.md.org"
       "/Users/djs30/emacs/org/Import/Archive/FluidCloud.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Handover.md.org"
       "/Users/djs30/emacs/org/Import/Archive/How we plan our work.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Platform2 Notes.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Project Artemis.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Renato PDP Notes 2022.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Sky Day - Scotland 2024.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Technology Products Mission Statement.md.org"
       "/Users/djs30/emacs/org/Import/Archive/Workload Atlas Notes.md.org"
       "/Users/djs30/emacs/org/Import/Archive/_Technology Products MOC.md.org"
       "/Users/djs30/emacs/org/Import/Templates/Meeting Note.md.org"
       "/Users/djs30/emacs/org/Import/Templates/Person.md.org"
       "/Users/djs30/emacs/org/Import/Templates/Quick Note.md.org"
       "/Users/djs30/emacs/org/Import/Templates/Weekly 5-15.md.org"
       "/Users/djs30/emacs/org/Import/_Inbox/2024-09-09 Technology Products 515.md.org"
       "/Users/djs30/emacs/org/Import/_Inbox/IMP - Cost Code Change.md.org"
       "/Users/djs30/emacs/org/Import/_Inbox/Meeting Note.md.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-02.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-05.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-06.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-07.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-08.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-09.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-12.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-13.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-19.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-21.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-22.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-26.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-06-27.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-03.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-04.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-05.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-06.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-07.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-12.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-13.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-14.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-17.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-18.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-19.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-20.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-21.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-24.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-25.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-26.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-27.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-28.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-07-31.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-01.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-03.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-04.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-14.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-15.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-16.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-17.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-18.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-21.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-22.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-23.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-24.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-25.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-28.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-29.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-30.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-08-31.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-01.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-04.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-05.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-06.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-07.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-08.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-11.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-12.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-13.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-14.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-15.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-18.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-19.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-20.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-21.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-22.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-28.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-09-29.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-12-08.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2023-12-12.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-04.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-11.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-19.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-21.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-22.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-23.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-24.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-25.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-26.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-29.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-30.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-01-31.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-02-01.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-04-30.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-08-26.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-08-27.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-09-02.org"
       "/Users/djs30/emacs/org/Import/roam/daily/2024-09-13.org"
       "/Users/djs30/emacs/org/Import/roam/daily/CAPTURE-2024-01-29.org"
       "/Users/djs30/emacs/org/Import/roam/20230602105758-bhargav_mannem.org"
       "/Users/djs30/emacs/org/Import/roam/20230602105856-prasanth_anandan.org"
       "/Users/djs30/emacs/org/Import/roam/20230602105923-rama_allamraju.org"
       "/Users/djs30/emacs/org/Import/roam/20230602110036-james_forbes.org"
       "/Users/djs30/emacs/org/Import/roam/20230602110159-watchman.org"
       "/Users/djs30/emacs/org/Import/roam/20230602110937-graham_brown.org"
       "/Users/djs30/emacs/org/Import/roam/20230602110952-shirley_harris.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111040-katie_fowler.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111235-craig_robertson.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111322-davide_tonelli.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111334-cks.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111343-cloudgrid.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111404-fluidcloud.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111534-milad_aldin_taamneh.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111554-sascha_eckhardt.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111607-vince_marco.org"
       "/Users/djs30/emacs/org/Import/roam/20230602111628-james_cruickshank.org"
       "/Users/djs30/emacs/org/Import/roam/20230602132127-fluidcloud_5_15.org"
       "/Users/djs30/emacs/org/Import/roam/20230602133948-matthew_beckett.org"
       "/Users/djs30/emacs/org/Import/roam/20230602134002-luca_nunzi.org"
       "/Users/djs30/emacs/org/Import/roam/20230602134107-martin_begley.org"
       "/Users/djs30/emacs/org/Import/roam/20230602140107-dne_cni_migration.org"
       "/Users/djs30/emacs/org/Import/roam/20230602140414-kubernetes_platform_stabilisation_improvements.org"
       "/Users/djs30/emacs/org/Import/roam/20230602140613-anicka_lewis.org"
       "/Users/djs30/emacs/org/Import/roam/20230602141908-watermark_house.org"
       "/Users/djs30/emacs/org/Import/roam/20230602142708-judy_wu.org"
       "/Users/djs30/emacs/org/Import/roam/20230602142816-elizabeth_riley.org"
       "/Users/djs30/emacs/org/Import/roam/20230602142920-nimbus.org"
       "/Users/djs30/emacs/org/Import/roam/20230605101535-harbor.org"
       "/Users/djs30/emacs/org/Import/roam/20230605111717-renato_paco.org"
       "/Users/djs30/emacs/org/Import/roam/20230605111741-runway.org"
       "/Users/djs30/emacs/org/Import/roam/20230605111815-technology_products.org"
       "/Users/djs30/emacs/org/Import/roam/20230605111827-colin_gordon.org"
       "/Users/djs30/emacs/org/Import/roam/20230605112407-ben_neise.org"
       "/Users/djs30/emacs/org/Import/roam/20230605112517-darren_jones.org"
       "/Users/djs30/emacs/org/Import/roam/20230605112556-janice_geddes.org"
       "/Users/djs30/emacs/org/Import/roam/20230605140738-andrew_murray.org"
       "/Users/djs30/emacs/org/Import/roam/20230605140802-matthew_mackmin.org"
       "/Users/djs30/emacs/org/Import/roam/20230605140819-steve_singh.org"
       "/Users/djs30/emacs/org/Import/roam/20230605140832-daniel_hough.org"
       "/Users/djs30/emacs/org/Import/roam/20230605140930-henry_truong.org"
       "/Users/djs30/emacs/org/Import/roam/20230605140942-luke_hackett.org"
       "/Users/djs30/emacs/org/Import/roam/20230605152700-ice.org"
       "/Users/djs30/emacs/org/Import/roam/20230605152717-tpv.org"
       "/Users/djs30/emacs/org/Import/roam/20230605152724-ciec.org"
       "/Users/djs30/emacs/org/Import/roam/20230605152742-john_naysmith.org"
       "/Users/djs30/emacs/org/Import/roam/20230606100228-imp.org"
       "/Users/djs30/emacs/org/Import/roam/20230607150409-pietro_de_meo.org"
       "/Users/djs30/emacs/org/Import/roam/20230608090920-tsa.org"
       "/Users/djs30/emacs/org/Import/roam/20230608090942-vdc.org"
       "/Users/djs30/emacs/org/Import/roam/20230608091016-project_gloria.org"
       "/Users/djs30/emacs/org/Import/roam/20230608092541-kalaiselvan_sekar.org"
       "/Users/djs30/emacs/org/Import/roam/20230608145842-ppc.org"
       "/Users/djs30/emacs/org/Import/roam/20230608145855-adtech.org"
       "/Users/djs30/emacs/org/Import/roam/20230608150716-clarity.org"
       "/Users/djs30/emacs/org/Import/roam/20230608162603-f0_2024_finance.org"
       "/Users/djs30/emacs/org/Import/roam/20230608164256-leigh_conner.org"
       "/Users/djs30/emacs/org/Import/roam/20230608164418-usha_nandhini_gnanaprakasam.org"
       "/Users/djs30/emacs/org/Import/roam/20230608164514-emily_hart.org"
       "/Users/djs30/emacs/org/Import/roam/20230608164707-david_kerr.org"
       "/Users/djs30/emacs/org/Import/roam/20230608164843-comcast.org"
       "/Users/djs30/emacs/org/Import/roam/20230608165210-james_davidson.org"
       "/Users/djs30/emacs/org/Import/roam/20230609104026-cd_controller.org"
       "/Users/djs30/emacs/org/Import/roam/20230609131025-martin_harrower.org"
       "/Users/djs30/emacs/org/Import/roam/20230619090444-mark_niven.org"
       "/Users/djs30/emacs/org/Import/roam/20230619101613-tech_products_reorg_2023.org"
       "/Users/djs30/emacs/org/Import/roam/20230619144001-slingshot.org"
       "/Users/djs30/emacs/org/Import/roam/20230630145003-donald_forbes.org"
       "/Users/djs30/emacs/org/Import/roam/20230703104455-anne_marie_lacy.org"
       "/Users/djs30/emacs/org/Import/roam/20230705105242-corin_thorpe.org"
       "/Users/djs30/emacs/org/Import/roam/20230706120816-tanzu_engineering_day_july_2023.org"
       "/Users/djs30/emacs/org/Import/roam/20230706125330-dne.org"
       "/Users/djs30/emacs/org/Import/roam/20230706125811-vantage.org"
       "/Users/djs30/emacs/org/Import/roam/20230706130929-playout.org"
       "/Users/djs30/emacs/org/Import/roam/20230707164030-2024_objectives.org"
       "/Users/djs30/emacs/org/Import/roam/20230709141204-test_page.org"
       "/Users/djs30/emacs/org/Import/roam/20230829093806-adam_thornburn.org"
       "/Users/djs30/emacs/org/Import/roam/20230829093857-ukis.org"
       "/Users/djs30/emacs/org/Import/roam/20230829094839-prem_solomon.org"
       "/Users/djs30/emacs/org/Import/roam/20230829095234-andrew_mowlam.org"
       "/Users/djs30/emacs/org/Import/roam/20230829101927-bryan_ross.org"
       "/Users/djs30/emacs/org/Import/roam/20230830150911-project_purple.org"
       "/Users/djs30/emacs/org/Import/roam/20230831100653-pedro_ferreira.org"
       "/Users/djs30/emacs/org/Import/roam/20230831103439-thoiba_thoudam.org"
       "/Users/djs30/emacs/org/Import/roam/20230905131704-shauna_smith.org"
       "/Users/djs30/emacs/org/Import/roam/20230906103529-isaac_martinez.org"
       "/Users/djs30/emacs/org/Import/roam/20230914104035-matthew_reeves.org"
       "/Users/djs30/emacs/org/Import/roam/20230914104049-toby_hersey.org"
       "/Users/djs30/emacs/org/Import/roam/20230914115417-martyn_hughes.org"
       "/Users/djs30/emacs/org/Import/roam/20230914115436-dave_burgess.org"
       "/Users/djs30/emacs/org/Import/roam/20230914115452-darran_rice.org"
       "/Users/djs30/emacs/org/Import/roam/20230914115516-marc_zottner.org"
       "/Users/djs30/emacs/org/Import/roam/20230914115531-jesse_bean.org"
       "/Users/djs30/emacs/org/Import/roam/20230919123513-massimiliano_barbero.org"
       "/Users/djs30/emacs/org/Import/roam/20240124105121-srisivatheepan_vijeyakumaran.org"
       "/Users/djs30/emacs/org/Import/roam/20240124105130-richard_osborne.org"
       "/Users/djs30/emacs/org/Import/roam/20240124131856-tyrone_donnelly.org"
       "/Users/djs30/emacs/org/Import/roam/20240124132208-ashley_moore.org"
       "/Users/djs30/emacs/org/Import/roam/20240124132907-matt_duckworth.org"
       "/Users/djs30/emacs/org/Import/roam/20240124160711-project_gold.org"
       "/Users/djs30/emacs/org/Import/roam/20240129082613-akbar_rashid.org"
       "/Users/djs30/emacs/org/Import/roam/20240129100417-alastair_davie.org"
       "/Users/djs30/emacs/org/Import/roam/20240130102832-rashee_kapoor.org"
       "/Users/djs30/emacs/org/Import/roam/20240131111932-mike_scott.org"
       "/Users/djs30/emacs/org/Import/roam/20240131161158-charles_bitter.org"
       "/Users/djs30/emacs/org/Import/roam/20240131161210-chase_bennett.org"
       "/Users/djs30/emacs/org/Import/roam/20240131161232-hans_thomsen.org"
       "/Users/djs30/emacs/org/Import/roam/20240827095104-brendan_lynch.org"
       "/Users/djs30/emacs/org/Import/roam/CAPTURE-20230608164256-leigh_conner.org"
       "/Users/djs30/emacs/org/Import/roam/CAPTURE-20230630145003-donald_forbes.org"
       "/Users/djs30/emacs/org/Import/roam/CAPTURE-20230703104455-anne_marie_lacy.org"
       "/Users/djs30/emacs/org/Import/Callouts.md.org"
       "/Users/djs30/emacs/org/Import/This is a typing test.md.org"
       "/Users/djs30/emacs/org/Import/inbox.org"
       "/Users/djs30/emacs/org/Import/meetings.org"
       "/Users/djs30/emacs/org/Import/reminders.org"
       "/Users/djs30/emacs/org/Import/tasks.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-02.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-05.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-06.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-07.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-08.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-09.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-12.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-13.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-19.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-21.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-22.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-26.org"
       "/Users/djs30/emacs/org/roam/daily/2023-06-27.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-03.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-04.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-05.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-06.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-07.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-12.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-13.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-14.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-17.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-18.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-19.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-20.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-21.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-24.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-25.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-26.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-27.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-28.org"
       "/Users/djs30/emacs/org/roam/daily/2023-07-31.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-01.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-03.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-04.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-14.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-15.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-16.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-17.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-18.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-21.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-22.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-23.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-24.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-25.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-28.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-29.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-30.org"
       "/Users/djs30/emacs/org/roam/daily/2023-08-31.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-01.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-04.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-05.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-06.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-07.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-08.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-11.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-12.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-13.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-14.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-15.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-18.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-19.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-20.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-21.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-22.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-28.org"
       "/Users/djs30/emacs/org/roam/daily/2023-09-29.org"
       "/Users/djs30/emacs/org/roam/daily/2023-12-08.org"
       "/Users/djs30/emacs/org/roam/daily/2023-12-12.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-04.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-11.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-19.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-21.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-22.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-23.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-24.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-25.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-26.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-29.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-30.org"
       "/Users/djs30/emacs/org/roam/daily/2024-01-31.org"
       "/Users/djs30/emacs/org/roam/daily/2024-02-01.org"
       "/Users/djs30/emacs/org/roam/daily/2024-04-30.org"
       "/Users/djs30/emacs/org/roam/daily/2024-08-26.org"
       "/Users/djs30/emacs/org/roam/daily/2024-08-27.org"
       "/Users/djs30/emacs/org/roam/daily/2024-09-02.org"
       "/Users/djs30/emacs/org/roam/daily/2024-09-17.org"
       "/Users/djs30/emacs/org/roam/daily/CAPTURE-2024-01-29.org"
       "/Users/djs30/emacs/org/roam/20230602105758-bhargav_mannem.org"
       "/Users/djs30/emacs/org/roam/20230602105856-prasanth_anandan.org"
       "/Users/djs30/emacs/org/roam/20230602105923-rama_allamraju.org"
       "/Users/djs30/emacs/org/roam/20230602110036-james_forbes.org"
       "/Users/djs30/emacs/org/roam/20230602110159-watchman.org"
       "/Users/djs30/emacs/org/roam/20230602110952-shirley_harris.org"
       "/Users/djs30/emacs/org/roam/20230602111040-katie_fowler.org"
       "/Users/djs30/emacs/org/roam/20230602111235-craig_robertson.org"
       "/Users/djs30/emacs/org/roam/20230602111334-cks.org"
       "/Users/djs30/emacs/org/roam/20230602111343-cloudgrid.org"
       "/Users/djs30/emacs/org/roam/20230602111404-fluidcloud.org"
       "/Users/djs30/emacs/org/roam/20230602111534-milad_aldin_taamneh.org"
       "/Users/djs30/emacs/org/roam/20230602111554-sascha_eckhardt.org"
       "/Users/djs30/emacs/org/roam/20230602111607-vince_marco.org"
       "/Users/djs30/emacs/org/roam/20230602111628-james_cruickshank.org"
       "/Users/djs30/emacs/org/roam/20230602132127-fluidcloud_5_15.org"
       "/Users/djs30/emacs/org/roam/20230602133948-matthew_beckett.org"
       "/Users/djs30/emacs/org/roam/20230602134002-luca_nunzi.org"
       "/Users/djs30/emacs/org/roam/20230602134107-martin_begley.org"
       "/Users/djs30/emacs/org/roam/20230602140107-dne_cni_migration.org"
       "/Users/djs30/emacs/org/roam/20230602140414-kubernetes_platform_stabilisation_improvements.org"
       "/Users/djs30/emacs/org/roam/20230602140613-anicka_lewis.org"
       "/Users/djs30/emacs/org/roam/20230602141908-watermark_house.org"
       "/Users/djs30/emacs/org/roam/20230602142708-judy_wu.org"
       "/Users/djs30/emacs/org/roam/20230602142816-elizabeth_riley.org"
       "/Users/djs30/emacs/org/roam/20230602142920-nimbus.org"
       "/Users/djs30/emacs/org/roam/20230605101535-harbor.org"
       "/Users/djs30/emacs/org/roam/20230605111717-renato_paco.org"
       "/Users/djs30/emacs/org/roam/20230605111741-runway.org"
       "/Users/djs30/emacs/org/roam/20230605111815-technology_products.org"
       "/Users/djs30/emacs/org/roam/20230605111827-colin_gordon.org"
       "/Users/djs30/emacs/org/roam/20230605112407-ben_neise.org"
       "/Users/djs30/emacs/org/roam/20230605112517-darren_jones.org"
       "/Users/djs30/emacs/org/roam/20230605112556-janice_geddes.org"
       "/Users/djs30/emacs/org/roam/20230605140738-andrew_murray.org"
       "/Users/djs30/emacs/org/roam/20230605140802-matthew_mackmin.org"
       "/Users/djs30/emacs/org/roam/20230605140819-steve_singh.org"
       "/Users/djs30/emacs/org/roam/20230605140832-daniel_hough.org"
       "/Users/djs30/emacs/org/roam/20230605140930-henry_truong.org"
       "/Users/djs30/emacs/org/roam/20230605140942-luke_hackett.org"
       "/Users/djs30/emacs/org/roam/20230605152700-ice.org"
       "/Users/djs30/emacs/org/roam/20230605152717-tpv.org"
       "/Users/djs30/emacs/org/roam/20230605152724-ciec.org"
       "/Users/djs30/emacs/org/roam/20230605152742-john_naysmith.org"
       "/Users/djs30/emacs/org/roam/20230606100228-imp.org"
       "/Users/djs30/emacs/org/roam/20230607150409-pietro_de_meo.org"
       "/Users/djs30/emacs/org/roam/20230608090920-tsa.org"
       "/Users/djs30/emacs/org/roam/20230608090942-vdc.org"
       "/Users/djs30/emacs/org/roam/20230608091016-project_gloria.org"
       "/Users/djs30/emacs/org/roam/20230608092541-kalaiselvan_sekar.org"
       "/Users/djs30/emacs/org/roam/20230608145842-ppc.org"
       "/Users/djs30/emacs/org/roam/20230608145855-adtech.org"
       "/Users/djs30/emacs/org/roam/20230608150716-clarity.org"
       "/Users/djs30/emacs/org/roam/20230608162603-f0_2024_finance.org"
       "/Users/djs30/emacs/org/roam/20230608164256-leigh_conner.org"
       "/Users/djs30/emacs/org/roam/20230608164418-usha_nandhini_gnanaprakasam.org"
       "/Users/djs30/emacs/org/roam/20230608164514-emily_hart.org"
       "/Users/djs30/emacs/org/roam/20230608164707-david_kerr.org"
       "/Users/djs30/emacs/org/roam/20230608164843-comcast.org"
       "/Users/djs30/emacs/org/roam/20230608165210-james_davidson.org"
       "/Users/djs30/emacs/org/roam/20230609104026-cd_controller.org"
       "/Users/djs30/emacs/org/roam/20230609131025-martin_harrower.org"
       "/Users/djs30/emacs/org/roam/20230619090444-mark_niven.org"
       "/Users/djs30/emacs/org/roam/20230619101613-tech_products_reorg_2023.org"
       "/Users/djs30/emacs/org/roam/20230619144001-slingshot.org"
       "/Users/djs30/emacs/org/roam/20230630145003-donald_forbes.org"
       "/Users/djs30/emacs/org/roam/20230703104455-anne_marie_lacy.org"
       "/Users/djs30/emacs/org/roam/20230705105242-corin_thorpe.org"
       "/Users/djs30/emacs/org/roam/20230706120816-tanzu_engineering_day_july_2023.org"
       "/Users/djs30/emacs/org/roam/20230706125330-dne.org"
       "/Users/djs30/emacs/org/roam/20230706125811-vantage.org"
       "/Users/djs30/emacs/org/roam/20230706130929-playout.org"
       "/Users/djs30/emacs/org/roam/20230707164030-2024_objectives.org"
       "/Users/djs30/emacs/org/roam/20230709141204-test_page.org"
       "/Users/djs30/emacs/org/roam/20230829093806-adam_thornburn.org"
       "/Users/djs30/emacs/org/roam/20230829093857-ukis.org"
       "/Users/djs30/emacs/org/roam/20230829094839-prem_solomon.org"
       "/Users/djs30/emacs/org/roam/20230829095234-andrew_mowlam.org"
       "/Users/djs30/emacs/org/roam/20230829101927-bryan_ross.org"
       "/Users/djs30/emacs/org/roam/20230830150911-project_purple.org"
       "/Users/djs30/emacs/org/roam/20230831100653-pedro_ferreira.org"
       "/Users/djs30/emacs/org/roam/20230831103439-thoiba_thoudam.org"
       "/Users/djs30/emacs/org/roam/20230905131704-shauna_smith.org"
       "/Users/djs30/emacs/org/roam/20230906103529-isaac_martinez.org"
       "/Users/djs30/emacs/org/roam/20230914104035-matthew_reeves.org"
       "/Users/djs30/emacs/org/roam/20230914104049-toby_hersey.org"
       "/Users/djs30/emacs/org/roam/20230914115417-martyn_hughes.org"
       "/Users/djs30/emacs/org/roam/20230914115436-dave_burgess.org"
       "/Users/djs30/emacs/org/roam/20230914115452-darran_rice.org"
       "/Users/djs30/emacs/org/roam/20230914115516-marc_zottner.org"
       "/Users/djs30/emacs/org/roam/20230914115531-jesse_bean.org"
       "/Users/djs30/emacs/org/roam/20230919123513-massimiliano_barbero.org"
       "/Users/djs30/emacs/org/roam/20240124105121-srisivatheepan_vijeyakumaran.org"
       "/Users/djs30/emacs/org/roam/20240124105130-richard_osborne.org"
       "/Users/djs30/emacs/org/roam/20240124131856-tyrone_donnelly.org"
       "/Users/djs30/emacs/org/roam/20240124132208-ashley_moore.org"
       "/Users/djs30/emacs/org/roam/20240124132907-matt_duckworth.org"
       "/Users/djs30/emacs/org/roam/20240124160711-project_gold.org"
       "/Users/djs30/emacs/org/roam/20240129082613-akbar_rashid.org"
       "/Users/djs30/emacs/org/roam/20240129100417-alastair_davie.org"
       "/Users/djs30/emacs/org/roam/20240130102832-rashee_kapoor.org"
       "/Users/djs30/emacs/org/roam/20240131111932-mike_scott.org"
       "/Users/djs30/emacs/org/roam/20240131161158-charles_bitter.org"
       "/Users/djs30/emacs/org/roam/20240131161210-chase_bennett.org"
       "/Users/djs30/emacs/org/roam/20240131161232-hans_thomsen.org"
       "/Users/djs30/emacs/org/roam/20240827095104-brendan_lynch.org"
       "/Users/djs30/emacs/org/roam/20240917150334-jamie_panagos.org"
       "/Users/djs30/emacs/org/roam/20240917150414-bill_mcclintok.org"
       "/Users/djs30/emacs/org/roam/20240917160635-rob_copsey.org"
       "/Users/djs30/emacs/org/roam/20240917160650-david_reid.org"
       "/Users/djs30/emacs/org/roam/20240917160657-holly_lander.org"
       "/Users/djs30/emacs/org/roam/CAPTURE-20230608164256-leigh_conner.org"
       "/Users/djs30/emacs/org/roam/CAPTURE-20230630145003-donald_forbes.org"
       "/Users/djs30/emacs/org/roam/CAPTURE-20230703104455-anne_marie_lacy.org"
       "/Users/djs30/emacs/org/5-15.org" "/Users/djs30/emacs/org/inbox.org"
       "/Users/djs30/emacs/org/reminders.org" "/Users/djs30/emacs/org/tasks.org"))
   '(package-selected-packages
     '(ace-jump-helm-line ace-link add-node-modules-path afternoon-theme
                          aggressive-indent alect-themes all-the-icons ample-theme
                          ample-zen-theme anti-zenburn-theme apropospriate-theme
                          auto-compile auto-dictionary auto-highlight-symbol
                          auto-yasnippet badwolf-theme
                          birds-of-paradise-plus-theme bubbleberry-theme
                          busybee-theme centered-cursor-mode cherry-blossom-theme
                          chocolate-theme clean-aindent-mode clues-theme
                          code-review color-theme-sanityinc-solarized
                          color-theme-sanityinc-tomorrow column-enforce-mode
                          company-web consult-org-roam counsel counsel-css
                          cyberpunk-theme dakrone-theme darkmine-theme
                          darkokai-theme darktooth-theme define-word devdocs
                          diminish dired-quick-sort disable-mouse django-theme
                          doom-themes dotenv-mode dracula-theme drag-stuff
                          dumb-jump ef-themes elisp-def elisp-demos
                          elisp-slime-nav emmet-mode emr espresso-theme
                          eval-sexp-fu evil-anzu evil-args evil-cleverparens
                          evil-collection evil-easymotion evil-escape
                          evil-evilified-state evil-exchange evil-goggles
                          evil-iedit-state evil-indent-plus evil-lion
                          evil-lisp-state evil-matchit evil-nerd-commenter
                          evil-numbers evil-org evil-surround evil-textobj-line
                          evil-tutor evil-unimpaired evil-visual-mark-mode
                          evil-visualstar exotica-theme expand-region eyebrowse
                          eziam-themes fancy-battery farmhouse-themes
                          flatland-theme flatui-theme flx-ido flycheck-elsa
                          flycheck-package flyspell-correct-helm gandalf-theme
                          gh-md git-link git-messenger git-modes git-timemachine
                          gitignore-templates gnuplot golden-ratio
                          google-translate gotham-theme grandshell-theme
                          gruber-darker-theme gruvbox-theme haml-mode
                          hc-zenburn-theme helm-ag helm-c-yasnippet helm-comint
                          helm-company helm-css-scss helm-descbinds helm-git-grep
                          helm-ls-git helm-make helm-mode-manager helm-org
                          helm-org-rifle helm-projectile helm-purpose helm-swoop
                          helm-themes helm-xref hemisu-theme heroku-theme
                          hide-comnt highlight-indentation highlight-numbers
                          highlight-parentheses hl-todo holy-mode htmlize
                          hungry-delete hybrid-mode impatient-mode indent-guide
                          info+ inkpot-theme inspector ir-black-theme ivy
                          jazz-theme jbeans-theme kaolin-themes light-soap-theme
                          link-hint lorem-ipsum lush-theme macrostep
                          madhat2r-theme majapahit-themes markdown-toc
                          material-theme minimal-theme modus-themes moe-theme
                          molokai-theme monochrome-theme monokai-theme multi-line
                          mustang-theme mwim nameless naquadah-theme
                          noctilux-theme obsidian-theme occidental-theme
                          oldlace-theme omtose-phellack-theme open-junk-file
                          org-cliplink org-contrib org-download org-mime org-msg
                          org-pomodoro org-present org-projectile org-rich-yank
                          org-roam-ui org-superstar organic-green-theme
                          orgit-forge overseer ox-pandoc ox-report pandoc-mode
                          paradox password-generator pcre2el
                          phoenix-dark-mono-theme phoenix-dark-pink-theme
                          planet-theme popwin prettier-js professional-theme
                          pug-mode purple-haze-theme quickrun railscasts-theme
                          rainbow-delimiters rebecca-theme request restart-emacs
                          reverse-theme sass-mode scss-mode seti-theme slim-mode
                          smeargle smyx-theme soft-charcoal-theme
                          soft-morning-theme soft-stone-theme solarized-theme
                          soothe-theme space-doc spacegray-theme spaceline
                          spacemacs-purpose-popwin spacemacs-whitespace-cleanup
                          string-edit-at-point string-inflection subatomic-theme
                          subatomic256-theme sublime-themes sunny-day-theme swiper
                          symbol-overlay symon tagedit tango-2-theme
                          tango-plus-theme tangotango-theme tao-theme term-cursor
                          toc-org toxi-theme treemacs-evil treemacs-icons-dired
                          treemacs-magit treemacs-persp treemacs-projectile
                          twilight-anti-bright-theme twilight-bright-theme
                          twilight-theme ujelly-theme underwater-theme undo-fu
                          undo-fu-session unfill vi-tilde-fringe vim-powerline
                          volatile-highlights vundo web-beautify
                          web-completion-data web-mode white-sand-theme winum
                          writeroom-mode ws-butler yasnippet-snippets
                          zen-and-art-theme zenburn-theme zonokai-emacs)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
