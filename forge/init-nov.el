;;;; init-nov.el
;;;; Emacs config to setup nov for reading ebooks

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-nov)
