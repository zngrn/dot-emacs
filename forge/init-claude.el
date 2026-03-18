;;;; init-claude.el

;;;; Claude Code IDE integration via MCP

;; Dependencies
(use-package websocket :ensure t)
(use-package web-server :ensure t)

;; Load from site-lisp
(add-to-list 'load-path
             (expand-file-name "site-lisp/claude-code-ide" user-emacs-directory))

(use-package claude-code-ide
  :ensure nil
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  ;; Use vterm as terminal backend
  (setq claude-code-ide-terminal-backend 'vterm)

  ;; Window layout
  (setq claude-code-ide-use-side-window t
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 90
        claude-code-ide-focus-on-open t)

  ;; Use ediff for reviewing Claude's suggestions
  (setq claude-code-ide-use-ide-diff t)

  ;; Expose Emacs tools to Claude via MCP
  (claude-code-ide-emacs-tools-setup))

(provide 'init-claude)
