;;;; init-appearance.el

;; Remove tool, menu & scroll bar
;; These are also suppressed in early-init.el for faster startup;
;; guard here for daemon/batch mode compatibility.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Remove startup message
(setq inhibit-startup-screen t)

;; Highlight the line on point - always
(global-hl-line-mode t)

;; Show column location of pointer
(column-number-mode)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Use golden ratio to resize windows based on focus, automatically
(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

;; Disable line numbers for some modes though
(dolist (mode '(term-mode-hook
                eshell-mode-hook
		shell-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font(s)
(set-face-attribute 'default nil
                    :family "Fira Code"
		    :height 138
                    :weight 'semi-light
                    :width 'semi-condensed)

;; Theme usage
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dark+ t))

;; Fullscreen is set in early-init.el to avoid small-window flash

(provide 'init-appearance)
