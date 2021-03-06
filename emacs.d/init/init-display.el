;; Hide menu, toolbar, and welcome message
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;; Load theme only when running on X
(when (display-graphic-p)
  ;; Solarized theme
  (load-theme 'solarized t)
  ;; Use the dark version of the theme
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized)
  ;; Powerline
  (require 'powerline)
  (powerline-default-theme))

;; Set font size
(set-face-attribute 'default nil :height 120) ; 120 * 1/10pt = 12pt

;; Buffer names management (cf `custom-set-variables')
(require 'uniquify)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Display tooltips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Smooth scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
;; (setq smooth-scroll-margin 5)
