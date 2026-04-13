;;;; init-code.el

;;; Handles code and similar domain related config

;; Maintian utf-8 standards
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use rainbowed delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO everywhere
(use-package hl-todo
  :config (global-hl-todo-mode t))

(defvar my/markdown-preview-header
  (expand-file-name "markdown-preview-header.html"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Pandoc --include-in-header file: base styles + mermaid bootstrap.")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Pandoc's -s already produces a standalone document, so markdown-mode's
  ;; `markdown-css-paths' / `markdown-xhtml-header-content' are ignored.
  ;; Pass CSS and the header (mermaid script) to pandoc directly instead.
  (setq markdown-command
        (mapconcat #'identity
                   (list "pandoc -s"
                         "--embed-resources --standalone"
                         "--highlight-style=pygments"
                         "--metadata title=Preview"
                         "--css=https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.css"
                         (format "--include-in-header=%s"
                                 (shell-quote-argument my/markdown-preview-header)))
                   " ")))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package yaml-mode)

(use-package json-mode
  :config
  (setq js-indent-level 2))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->"     . mc/mark-next-like-this)         ; add cursor at next match
         ("C-<"     . mc/mark-previous-like-this)     ; add cursor at previous match
         ("C-c m a" . mc/mark-all-like-this)           ; cursors on all matches
         ("C-c m l" . mc/edit-lines)                   ; cursor on each selected line
         ("C-c m r" . set-rectangular-region-anchor)))  ; rectangular selection

;; Code folding via hs-minor-mode (built-in)
;; Works with functions, classes, JSON objects, etc.
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'json-mode-hook 'hs-minor-mode)

;; Folding keybindings
(global-set-key (kbd "C-c f t") 'hs-toggle-hiding)    ; toggle at point
(global-set-key (kbd "C-c f a") 'hs-hide-all)         ; collapse all
(global-set-key (kbd "C-c f s") 'hs-show-all)         ; expand all
(global-set-key (kbd "C-c f l") 'hs-hide-level)       ; collapse to level

(provide 'init-code)
