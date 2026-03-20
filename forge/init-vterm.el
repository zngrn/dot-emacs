;;;; init-vterm.el

;;;; Terminal emulator using libvterm — full zsh integration

(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :init
  (setq vterm-shell "/bin/zsh"
        vterm-max-scrollback 10000
        vterm-buffer-name-string "vterm: %s")
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-copy-exclude-prompt t
        vterm-environment '("TERM=xterm-256color" "COLORTERM=truecolor"))

  ;; ARM Mac module compilation
  (when (eq system-type 'darwin)
    (setq vterm-module-cmake-args "-DUSE_CCACHE=Yes -DCMAKE_OSX_ARCHITECTURES=arm64"))

  ;; Disable line numbers in vterm buffers
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))

  :bind
  (("C-c t" . vterm)
   ("C-c T" . (lambda () (interactive) (vterm t)))  ; new vterm buffer
   :map vterm-mode-map
   ("S-<return>" . (lambda () (interactive) (vterm-send-string "\e[13;2u")))  ; Shift+Enter for Claude CLI newlines
   ("s-k"     . vterm-clear)              ; Cmd+k clears like in native terminal
   ("C-c C-c" . vterm-send-C-c)
   ("C-c C-d" . vterm-send-C-d)
   ("C-c C-l" . vterm-clear)
   ("M-["     . vterm-copy-mode)
   :map vterm-copy-mode-map
   ("q"       . vterm-copy-mode-done)))

(provide 'init-vterm)
