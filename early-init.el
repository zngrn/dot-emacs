;;;; early-init.el
;;;; Loaded before init.el and before the GUI frame initializes

;; Pause GC during startup for faster init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't initialize packages here — we do it manually in init.el
(setq package-enable-at-startup nil)

;; Prevent UI elements from flashing before init.el disables them
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
