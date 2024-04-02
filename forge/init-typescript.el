;;;; init-typescript.el

;;;; Handle typescript configuration

;; Use typescript major mode
(use-package typescript-mode
  :ensure t)

;; handle node path to pair with ts mode
(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)))

;; tide for typescript
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save))
  :config
  (setq tide-node-executable (executable-find "node"))
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

(provide 'init-typescript)

