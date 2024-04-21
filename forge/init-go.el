;;;; init-go.el

;;;; configure golang

(use-package go-mode
  :mode "\\.go\\'"
  :ensure t
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  )

;; path to gopls
(setq lsp-gopls-server-path "~/go/bin/gopls")

;; static analysis
(setq lsp-gopls-staticcheck t)

(provide 'init-go)
