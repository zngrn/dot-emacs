;;;; init.el
;;;; Emacs core config file

;; Maintian utf-8 standards
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Allow loading custom config from specified dir
(add-to-list 'load-path (expand-file-name "forge" user-emacs-directory))

;; Load individual init-*.el files here
(require 'init-appearance)
(require 'init-typescript)

;; Handling gc-handling on 50 MB
(setq gc-cons-threshold 50000000 gc-cons-percentage 0.6)

;; Reset GC to reasonable defaults
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; Type over selection
(delete-selection-mode 1)

;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Enable undo-tree
(global-undo-tree-mode)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Magit setup
(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off audible bell
(setq ring-bell-function 'ignore)

;; Use rainbowed delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Use elpy for python integration
(elpy-enable)

;; Neo-tree setup
(global-set-key (kbd "<f8>") 'neotree-toggle)
(add-hook 'emacs-startup-hook 'neotree-toggle)

;; to track command logs
(use-package command-log-mode)

;; ivy setup
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

(require 'counsel)

;; set up the doom mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
	   (doom-modeline-bar-width 4)
	   (doom-modeline-major-mode-color-icon t)
	   (doom-modeline-major-mode-icon t)
	   (doom-modeline-indent-info t)
	   (doom-modeline-project-detection 'relative-from-project)
	   (doom-modeline-minor-modes t)))

;; nyan mode
(use-package nyan-mode
  :init
  (nyan-mode))

;; cheat-sheet for key binding suggestions
;; delay load after 5 ms
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init
  (setq-default projectile-cache-file
                (expand-file-name ".projectile-cache" user-emacs-directory))
;;  (add-hook 'prog-mode-hook #'projectile-mode)

  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq-default projectile-enable-caching t
                projectile-mode-line-prefix ""
                projectile-sort-order 'recentf
                ;; Show project (if any) name in modeline
                projectile-mode-line '(:eval (projectile-project-name))))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Flycheck for syntax highlighting and checks
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Use company for autocomplete
(use-package company
  :ensure t
  :hook ((after-init . global-company-mode)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Fast-scrolling
(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

;; Highlight TODO everywhere
(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'linum-mode)
  (add-hook 'markdown-mode-hook 'linum-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package yaml-mode)

(use-package json-mode)

