;;;; init-appearance.el

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

;; Disable line numbers for some modes though
(dolist (mode '(term-mode-hook
                eshell-mode-hook
		shell-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font(s)
(set-face-attribute 'default nil
                    :family "Fira Code"
		    :height 135
                    :weight 'normal
                    :width 'normal)

;; Theme usage
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-wilmersdorf t))

;; Initial frame height and width to open to max window size
;; works well with external displays as well
(setq default-frame-alist '((fullscreen . maximized)))

(provide 'init-appearance)
