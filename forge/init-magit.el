;;;; init-magit

;;;; configuration for using magit

(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))

(provide 'init-magit)
