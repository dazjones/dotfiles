;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq default-directory "~/")

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
   '(evil-magit counsel-projectile vterm yaml-mode which-key visual-fill-column use-package simple-httpd projectile no-littering markdown-mode magit ivy-prescient hydra helpful general evil-collection doom-themes doom-modeline dired-single dired-open dired-hide-dotfiles dashboard counsel command-log-mode centaur-tabs auto-package-update all-the-icons-dired ace-window)))

(use-package no-littering)

(setq inhibit-startup-message t)

(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set font
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :config
  (general-create-definer dj/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
        :global-prefix "C-SPC"))

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-nord t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

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

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(setq org-directory "~/emacs/org/")

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
      visual-fill-column-center-text t)
(visual-fill-column-mode 1))

(use-package visual-fill-column
   :hook (org-mode . efs/org-mode-visual-fill))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0)
  (visual-line-mode 1))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files '("~/emacs/org/"))
  (setq org-default-notes-file "~/emacs/org/inbox.org")

  (with-eval-after-load 'org-capture
    ;; Empty template list to stop duplication on eval-buffer
    (setq org-capture-templates nil)
    
    (add-to-list 'org-capture-templates
	'("c" "Calendar Entry"  entry
	(file "~/emacs/org/calendar.org")
	    "* %(org-insert-time-stamp (org-read-date nil t))  %?" :empty-lines 1))
    
    (add-to-list 'org-capture-templates
	'("n" "Note"  entry
	(file org-default-notes-file)
	    "* NOTE:  %?" :empty-lines 1))

    (add-to-list 'org-capture-templates
	'("t" "Task"  entry
	(file "~/emacs/org/tasks.org")
	    "* TODO  %?" :empty-lines 1))
  
    (add-to-list 'org-capture-templates
	'("m" "Meeting Note"  entry
	(file "~/emacs/org/meeting-notes.org")
	    "* %^{What are we discussing?} \nWITH: %^{Who are you talking to?} \nTIMESTAMP:%U \n- %? " :empty-lines 1 :prepend t :clock-in t :clock-resume t :time-prompt t)))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "READY(r)" "ACTIVE(a)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
	     ;; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  )

;; Save backup files to a dedicated directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)

;; Make numeric backup versions unconditionally.
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Do not create lock files.
(setq create-lockfiles nil)

;; Use year/month/day
(setq calendar-date-style 'iso)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Watch and reload the file changed on the disk.
(global-auto-revert-mode +1)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(dj/leader-keys
 "g"   '(:ignore t :which-key "git")
 "gs"  'magit-status
 "gd"  'magit-diff-unstaged
 "gc"  'magit-branch-or-checkout
 "gl"   '(:ignore t :which-key "log")
 "glc" 'magit-log-current
 "glf" 'magit-log-buffer-file
 "gb"  'magit-branch
 "gP"  'magit-push-current
 "gp"  'magit-pull-branch
 "gf"  'magit-fetch
 "gF"  'magit-fetch-all
 "gr"  'magit-rebase)

(require 'dired-x)
(setq dired-listing-switches "-l --group-directories-first")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))
