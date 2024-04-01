;; Maintian utf-8 standards
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Handling gc-handling on 50 MB
(setq gc-cons-threshold 50000000 gc-cons-percentage 0.6)

;; Reset GC to reasonable defaults
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; Remove tool, menu & scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; Remove startup message
(setq inhibit-startup-screen t)

;; Highlight the line on point - always
(global-hl-line-mode t)

;; Show column location of pointer
(column-number-mode)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Type over selection
(delete-selection-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Enable undo-tree
(global-undo-tree-mode)

;; Font(s)
(set-face-attribute 'default nil
                    :family "Fira Code"
		    :height 120
                    :weight 'normal
                    :width 'normal)

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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-wilmersdorf t))

;; Initial frame height and width to open to max window size
(setq default-frame-alist '((fullscreen . maximized)))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off audible bell
(setq ring-bell-function 'ignore)

;; Use rainbowed delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show full path in title bar
(setq-default frame-title-format "%b (%f)")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hl-todo fast-scroll add-node-modules-path typescript-mode counsel-projectile which-key rainbow-delimiters counsel doom-modeline command-log-mode paredit ruby-test-mode neotree auto-complete undo-tree cider json-mode js2-mode tide clojure-mode elpy magit projectile flatui-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
	   (doom-modeline-minor-modes nil)))

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

;; Use typescript major mode
(use-package typescript-mode
  :ensure t)

(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)))

(setq tide-node-executable "/Users/vjhingran/.nvm/versions/node/v18.16.0/bin/node")

;; tide for typescript
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save))
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

;; Fast-scrolling
(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

;; Highlight TODO everywhere
(use-package hl-todo
  :config (global-hl-todo-mode t))

