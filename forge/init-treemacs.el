;;;; init-treemacs.el

;;;; configure treemacs setup

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
        treemacs-deferred-git-apply-delay      0.5
	treemacs-follow-mode                   t
        treemacs-directory-name-transformer    #'identity
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-file-name-transformer         #'identity
        treemacs-follow-after-init            t
        treemacs-follow-recenter-distance      0.1
        treemacs-goto-tag-strategy            'refetch-index
        treemacs-indentation                  2
        treemacs-indentation-string           " "
        treemacs-is-never-other-window        nil
        treemacs-max-git-entries              5000
        treemacs-missing-project-action       'ask
        treemacs-move-forward-on-expand       nil
        treemacs-no-png-images                nil
        treemacs-no-delete-other-windows      t
        treemacs-project-follow-cleanup       'always
        treemacs-persist-file                 (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                     'left
        treemacs-recenter-distance            0.1
        treemacs-recenter-after-file-follow   nil
        treemacs-recenter-after-tag-follow    nil
        treemacs-recenter-after-project-jump  'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                  nil
        treemacs-show-hidden-files            t
        treemacs-silent-filewatch             nil
        treemacs-silent-refresh               nil
        treemacs-sorting                      'alphabetic-asc
        treemacs-space-between-root-nodes     t
        treemacs-tag-follow-cleanup           t
        treemacs-tag-follow-delay             1.5
        treemacs-user-mode-line-format        nil
        treemacs-user-header-line-format      nil
        treemacs-width                        35
        treemacs-workspace-switch-cleanup     nil)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(provide 'init-treemacs)
