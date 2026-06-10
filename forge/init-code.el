;;;; init-code.el

;;; Handles code and similar domain related config

;; Maintian utf-8 standards
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use rainbowed delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO everywhere
(use-package hl-todo
  :config (global-hl-todo-mode t))

(defvar my/markdown-preview-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the pandoc preview asset files.")

(defun my/markdown-preview-asset (name)
  "Absolute path to a pandoc preview asset NAME in `my/markdown-preview-dir'."
  (expand-file-name name my/markdown-preview-dir))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Pandoc's -s already produces a standalone document, so markdown-mode's
  ;; `markdown-css-paths' / `markdown-xhtml-header-content' are ignored.
  ;; Pass CSS and the header (mermaid script) to pandoc directly instead.
  ;;
  ;; github-markdown-css scopes every rule under `.markdown-body', so the
  ;; content is wrapped in <article class="markdown-body"> via the
  ;; before/after-body includes -- without that wrapper none of the GitHub
  ;; styling (tables, code blocks, headings) applies. The CSS version is
  ;; pinned so an upstream release can't silently change the rendering.
  (setq markdown-command
        (mapconcat #'identity
                   (list "pandoc -s"
                         "--embed-resources --standalone"
                         "--highlight-style=pygments"
                         "--metadata title=Preview"
                         "--css=https://cdn.jsdelivr.net/npm/github-markdown-css@5.9.0/github-markdown.css"
                         (format "--include-in-header=%s"
                                 (shell-quote-argument
                                  (my/markdown-preview-asset "markdown-preview-header.html")))
                         (format "--include-before-body=%s"
                                 (shell-quote-argument
                                  (my/markdown-preview-asset "markdown-preview-before.html")))
                         (format "--include-after-body=%s"
                                 (shell-quote-argument
                                  (my/markdown-preview-asset "markdown-preview-after.html"))))
                   " "))
  :bind (:map markdown-mode-map
              ("C-c C-c r" . my/markdown-live-preview)
              ("C-c C-c R" . my/markdown-live-preview-stop)))

;;; Live browser preview ------------------------------------------------------
;; markdown-mode's `C-c C-c v' opens a one-shot static export. This serves the
;; current buffer -- rendered through the same `markdown-command' pandoc
;; pipeline (so identical CSS + mermaid) -- over a local simple-httpd server.
;; The page polls a version endpoint and reloads itself whenever the buffer is
;; saved or edited (after a brief idle), so the browser tracks the buffer live.
;; Scroll position is preserved across reloads.
;;   C-c C-c r  start / retarget live preview (r = reload)
;;   C-c C-c R  stop live preview + server

(defvar my/markdown-live--buffer nil
  "Source buffer currently driving the live markdown preview.")
(defvar my/markdown-live--html ""
  "Cached rendered HTML for the live preview, including the reload poller.")
(defvar my/markdown-live--version 0
  "Render counter; bumped on every re-render and polled by the browser.")
(defvar my/markdown-live--tick nil
  "`buffer-modified-tick' of the live buffer at the last render.")
(defvar my/markdown-live--timer nil
  "Idle timer that re-renders the live buffer after edits.")

(defun my/markdown-live--reload-snippet ()
  "Return a <script> that reloads the page when the render version changes."
  (format "
<script>
(function () {
  var current = %d;
  setInterval(function () {
    fetch('/preview-version', { cache: 'no-store' })
      .then(function (r) { return r.text(); })
      .then(function (v) {
        if (parseInt(v, 10) !== current) {
          try { sessionStorage.setItem('mdScroll', String(window.scrollY)); } catch (e) {}
          location.reload();
        }
      })
      .catch(function () {});
  }, 600);
  window.addEventListener('load', function () {
    var y = sessionStorage.getItem('mdScroll');
    if (y !== null) { window.scrollTo(0, parseInt(y, 10)); }
  });
})();
</script>" my/markdown-live--version))

(defun my/markdown-live--render ()
  "Render `my/markdown-live--buffer' via `markdown-command' and cache the HTML."
  (when (buffer-live-p my/markdown-live--buffer)
    (let ((out (generate-new-buffer " *markdown-live*")))
      (unwind-protect
          (progn
            (with-current-buffer my/markdown-live--buffer
              (setq my/markdown-live--tick (buffer-modified-tick))
              (call-process-region (point-min) (point-max)
                                   shell-file-name nil out nil
                                   shell-command-switch markdown-command))
            (setq my/markdown-live--version (1+ my/markdown-live--version)
                  my/markdown-live--html
                  (concat (with-current-buffer out (buffer-string))
                          (my/markdown-live--reload-snippet))))
        (kill-buffer out)))))

(defun my/markdown-live--idle-render ()
  "Re-render when the live buffer has changed since the last render."
  (when (and (buffer-live-p my/markdown-live--buffer)
             (not (eql (buffer-modified-tick my/markdown-live--buffer)
                       my/markdown-live--tick)))
    (my/markdown-live--render)))

;; `defservlet' is a macro from simple-httpd, so the servlets must be defined
;; only after the package has loaded -- hence inside `:config'. simple-httpd
;; itself loads lazily via the `httpd-start' autoload the first time a preview
;; is started.
(use-package simple-httpd
  :commands (httpd-start httpd-stop)
  :config
  (defservlet preview "text/html; charset=utf-8" ()
    (insert (if (> my/markdown-live--version 0)
                my/markdown-live--html
              (concat "<!doctype html><meta charset=\"utf-8\">"
                      "<body style=\"font-family:sans-serif;padding:2rem\">"
                      "<p>No live markdown preview is running. Open a markdown "
                      "buffer and run <code>C-c C-c r</code>.</p>"))))
  (defservlet preview-version "text/plain; charset=utf-8" ()
    (insert (number-to-string my/markdown-live--version))))

(defun my/markdown-live-preview ()
  "Start (or retarget) a live browser preview of the current markdown buffer."
  (interactive)
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a markdown buffer"))
  (setq my/markdown-live--buffer (current-buffer))
  (unless (get-process "httpd")
    (httpd-start))
  (my/markdown-live--render)
  (add-hook 'after-save-hook #'my/markdown-live--idle-render nil t)
  (unless my/markdown-live--timer
    (setq my/markdown-live--timer
          (run-with-idle-timer 0.6 t #'my/markdown-live--idle-render)))
  (browse-url (format "http://localhost:%d/preview" httpd-port))
  (message "Live markdown preview: http://localhost:%d/preview" httpd-port))

(defun my/markdown-live-preview-stop ()
  "Stop the live markdown preview and its server."
  (interactive)
  (when my/markdown-live--timer
    (cancel-timer my/markdown-live--timer)
    (setq my/markdown-live--timer nil))
  (setq my/markdown-live--buffer nil
        my/markdown-live--version 0
        my/markdown-live--html "")
  (when (get-process "httpd")
    (httpd-stop))
  (message "Live markdown preview stopped."))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package yaml-mode)

(use-package json-mode
  :config
  (setq js-indent-level 2))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->"     . mc/mark-next-like-this)         ; add cursor at next match
         ("C-<"     . mc/mark-previous-like-this)     ; add cursor at previous match
         ("C-c m a" . mc/mark-all-like-this)           ; cursors on all matches
         ("C-c m l" . mc/edit-lines)                   ; cursor on each selected line
         ("C-c m r" . set-rectangular-region-anchor)))  ; rectangular selection

;; Code folding via hs-minor-mode (built-in)
;; Works with functions, classes, JSON objects, etc.
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'json-mode-hook 'hs-minor-mode)

;; Folding keybindings
(global-set-key (kbd "C-c f t") 'hs-toggle-hiding)    ; toggle at point
(global-set-key (kbd "C-c f a") 'hs-hide-all)         ; collapse all
(global-set-key (kbd "C-c f s") 'hs-show-all)         ; expand all
(global-set-key (kbd "C-c f l") 'hs-hide-level)       ; collapse to level

(provide 'init-code)
