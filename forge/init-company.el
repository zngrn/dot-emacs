;;;; init-company.el

;;;; configure company for programming goodness

;; Use company for autocompletion in programming along with lsp-mode
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


;; prettify the suggestion box of company
(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'init-company)
