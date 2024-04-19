;;;; init-lsp.el

;;;; configure LSP handling for programming

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((clojure-mode . lsp)
	 (clojurec-mode . lsp)
	 (clojurescript-mode . lsp)
	 (web-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  ;; add paths to local installation of tools like lein
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH"))))

;; tree-view of code hierarchy
(use-package lsp-treemacs
  :after lsp)

;; jump to anything in a project by name lookup
(use-package lsp-ivy)

;; lsp-ui -> use when needed, enable it if required
(use-package lsp-ui
  :disabled t
  :commands lsp-ui-mode)

(provide 'init-lsp)
