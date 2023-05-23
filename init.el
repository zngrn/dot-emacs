;; Remove tool, menu & scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight the line on point - always
(global-hl-line-mode t)

;; Show line numbers
(global-linum-mode)

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

;; Theme
(load-theme 'flatui t)

;; Initial frame height and width
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 180))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off audible bell
(setq ring-bell-function 'ignore)

;; Show full path in title bar
(setq-default frame-title-format "%b (%f)")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(paredit ruby-test-mode neotree auto-complete undo-tree cider json-mode js2-mode tide clojure-mode elpy magit projectile flatui-theme)))
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

