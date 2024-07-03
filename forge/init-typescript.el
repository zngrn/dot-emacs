;;;; init-typescript.el

;;;; Handle typescript configuration

;; Use typescript major mode
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (setq lsp-clients-typescript-server 'typescript-language-server)
  (add-hook 'typescript-mode-hook 'paredit-mode))

;; handle node path to pair with ts mode
(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)))


(provide 'init-typescript)
