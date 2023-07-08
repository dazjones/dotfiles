;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Set up auto update for packages
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dashboard hydra ivy-prescient counsel ivy all-the-icons no-littering which-key helpful zenburn-theme org-roam org doom-modeline general evil-collection evil auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Keep .emacs.d clean
(use-package no-littering)

;; Set up directories
(setq org-directory "~/emacs/org/")
(setq org-agenda-files '("~/emacs/org/"))
(setq org-roam-directory "~/emacs/org/roam")

;; Tidy up the UI
(setq inhibit-startup-message t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

;; Set typography
(set-face-attribute 'default nil :font "Roboto Mono" :height 140 :weight 'light)
(set-face-attribute 'bold nil :font "Roboto Mono" :weight 'regular)

;; Set dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (dashboard-setup-startup-hook))

;; Set up global key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts

;; Be as evil as possible
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
 
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; General package for key bindings
(use-package general
  :after evil
  )

;; Set up org key bindings
(general-define-key
 :prefix "C-c"
 "a" '(nil :which-key "applications")
 "a o" '(nil :which-key "org")
 "a o a" 'org-agenda
 "a o c" 'org-capture
 )

;; Set up org-roam key bindings
(general-define-key
 :prefix "C-c a o r"
 "" '(nil :which-key "org-roam")
 "i" 'org-roam-node-insert
 "f" 'org-roam-node-find
 "d" '(nil :which-key "dailies")
 "d t" 'org-roam-dailies-goto-today
 "d y" 'org-roam-dailies-goto-yesterday
 )

;; Use the doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; which-key helpful UI
(use-package which-key
  :config
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;; Collect various Icon Fonts and propertize
(use-package all-the-icons)

;; Ivy gives auto-completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
    (ivy-prescient-mode 1))

;; Set up org
(use-package org
  :pin gnu
  :commands (org-capture org-agenda)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (with-eval-after-load 'org-capture
    ;; Empty template list to stop duplication on eval-buffer
    ;;(setq org-capture-templates nil)
  )
)

;; Set up org-roam
(use-package org-roam
  :disabled
  :after (org)
  :init
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory 'org-roam-directory)
)
