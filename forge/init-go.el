;;;; init-go.el

;;;; configure golang

(use-package go-mode
  :mode "\\.go\\'"
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (add-hook 'go-mode-hook 'smartparens-mode))

;; path to gopls
(setq lsp-gopls-server-path "~/go/bin/gopls")

;; static analysis
(setq lsp-gopls-staticcheck t)

;; leverage lsp for go
(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)

(provide 'init-go)
