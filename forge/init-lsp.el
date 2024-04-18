;;;; init-lsp.el

;;;; configure LSP handling for programming

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; tree-view of code hierarchy
(use-package lsp-treemacs
  :after lsp)

;; jump to anything in a project by name lookup
(use-package lsp-ivy)

(provide 'init-lsp)
