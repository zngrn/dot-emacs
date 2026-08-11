;;;; init.el
;;;; Emacs core config file

;; Allow loading custom config from specified dir
(add-to-list 'load-path (expand-file-name "forge" user-emacs-directory))

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

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; flx scores ivy's fuzzy matches so basename and word-boundary hits rank
;; first. Ivy checks for it once at load time, and init-lsp pulls in ivy
;; via lsp-ivy, so flx must load before the init-* requires below.
(use-package flx)

;; Load individual init-*.el files here
(require 'init-appearance)
(require 'init-code)
(require 'init-lsp)
(require 'init-company)
(require 'init-treemacs)
(require 'init-clojure)
(require 'init-typescript)
(require 'init-go)
(require 'init-magit)
(require 'init-nov)
(require 'init-vterm)
(require 'init-claude)

;; Reset GC to reasonable defaults after startup
;; (GC is paused in early-init.el for fast startup)
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
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

;; Disable creating lock files
(setq create-lockfiles nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off audible bell
(setq ring-bell-function 'ignore)

;; Show full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Use elpy for python integration (deferred until python-mode)
(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before (lambda (&rest _) (elpy-enable))))

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
  (ivy-mode 1)
  ;; Loose subsequence matching for project file finding: "mph" matches
  ;; markdown-preview-header.html. Everything else (swiper, M-x, ...)
  ;; keeps the default matcher so results stay precise.
  (setq ivy-re-builders-alist
        '((counsel-projectile-find-file . ivy--regex-fuzzy)
          (counsel-projectile . ivy--regex-fuzzy)
          (projectile-completing-read . ivy--regex-fuzzy)
          (t . ivy--regex-plus))
        ;; flx only sorts matches when they fit under this cap; the default
        ;; of 200 means no smart ordering in a large project until the
        ;; candidate list is already narrowed way down
        ivy-flx-limit 5000))

(use-package counsel
  :ensure t
  :config (counsel-mode 1))

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
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-mode-line-prefix ""
        projectile-sort-order 'default
        ;; doom-modeline already shows project name — skip duplicate detection
        projectile-mode-line ""))

(use-package counsel-projectile
  ;; :demand keeps the package loading eagerly — :bind alone would defer it,
  ;; leaving counsel-projectile-mode off until the first s-p press
  :demand t
  :bind (("s-p" . counsel-projectile-find-file)) ; quick-open on Cmd+P
  :config (counsel-projectile-mode))

;; Frecency sorting: files picked often/recently float to the top, so an
;; empty Cmd+P prompt starts with recent files
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil) ; sorting only; matching stays with flx
  :config
  (prescient-persist-mode 1) ; remember choices across restarts
  (ivy-prescient-mode 1))

;; File icons + dimmed metadata columns in ivy lists
;; (nerd-icons-ivy-rich-mode must be enabled before ivy-rich-mode)
(use-package nerd-icons-ivy-rich
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))

;; Flycheck for syntax highlighting and checks
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Fast-scrolling
(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))
;; Custom settings live in custom.el — do not duplicate here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
