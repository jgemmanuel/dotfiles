;; -*- mode: emacs-lisp -*-
;; Emacs configuration init.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add user path

(add-to-list 'load-path "~/.emacs.d/init")

(load "init-package")			; package management
(load "init-display")			; frame appearance settings
(load "init-smartparens")		; smartparens settings
(load "init-org")			; org-mode settings
(load "init-typescript")		; typescript-settings
(load "init-templates")			; inserting templates

;; Enable ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Configure yasnippet and auto-complete
;; Credit: https://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
;; Note: auto complete mod should be loaded after yasnippet so that they can
;; work together
(yas-global-mode 1)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; Set TeX input method as default (C-\ to toggle)
;; Also try C-x 8 C-h
;; Credit: @MarcinBorkowski (http://mbork.pl/2014-09-13_TeX_input_method)
(setq default-input-method "TeX")

;; js-mode
(setq js-indent-level 2)
(add-hook 'js-mode-hook 'custom-js2-mode-hook)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(defun custom-js2-mode-hook ()
  (when (and (stringp buffer-file-name)
             (string-match "\.js$" buffer-file-name))
    (js2-minor-mode)))

;; ace-jump-mode
;; "C-c SPC" --> ace-jump-word-mode
;; "C-u C-c SPC" --> ace-jump-char-mode
;; "C-u C-u C-c SPC" --> ace-jump-line-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Set width of screen for the purpose of word-wrapping (enable `auto-fill-mode')
(setq-default fill-column 78)

;; Auto indentation (Not supported by some modes, e.g. f90-mode)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Kill the newline between indented lines and remove extra spaces caused by indentation
(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1)
	     (kill-line arg))
    (kill-line arg)))
;; Bind `kill-and-join-forward' to `C-k'
(global-set-key "\C-k" 'kill-and-join-forward)

;; Delete trailing whitespace in buffer upon save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Java-mode untabify the buffer upon save
;; Credit: @ian eure (http://stackoverflow.com/a/322690)
(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))
(defun progmodes-hooks ()
  "Hooks for programming modes"
  ;; (yas/minor-mode-on)
  (add-hook 'before-save-hook 'progmodes-write-hooks))
(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (untabify-buffer)))
(add-hook 'java-mode-hook 'progmodes-hooks)

;; Toggle `linum-mode' (cf `custom-set-variables')
(global-set-key (kbd "<f7>") 'linum-mode)

;; shell-mode (fix regarding garbled characters)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; fly-spell-mode
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'jade-mode-hook 'turn-on-flyspell)

;; Ask for y/n keypress instead of typing out `yes' or `no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Autolad Octave mode for *.m files and change default comment character to `%'
(autoload 'octave-mode "octave-mod" nil t)
(define-derived-mode custom-octave-mode octave-mode "CustomOctaveMode"
  "Comments start with `%'."
  (set (make-local-variable 'comment-start) "%"))
(setq auto-mode-alist
      (cons '("\\.m$" . custom-octave-mode) auto-mode-alist))

;; Toggle window split (Only works for two windows within single frame)
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
;; Assign it to C-x 4 t
(define-key ctl-x-4-map "t" 'toggle-window-split)

;; Windmove
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; buffer-move keybinds
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Define escape sequences for accessibility on tty
(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;2C" [S-right])
(define-key input-decode-map "\e[1;2D" [S-left])
(define-key input-decode-map "\e[1;3A" [M-up])
(define-key input-decode-map "\e[1;3B" [M-down])
(define-key input-decode-map "\e[1;3C" [M-right])
(define-key input-decode-map "\e[1;3D" [M-left])
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])
(define-key input-decode-map "\e[1;4A" [S-M-up])
(define-key input-decode-map "\e[1;4B" [S-M-down])
(define-key input-decode-map "\e[1;4C" [S-M-right])
(define-key input-decode-map "\e[1;4D" [S-M-left])



;;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(linum-format (quote "%3d"))
 '(org-agenda-files nil)
 '(org-calc-default-modes
   (quote
    (calc-internal-prec 20 calc-float-format
			(float 8)
			calc-angle-mode rad calc-prefer-frac nil calc-symbolic-mode nil calc-date-format
			(YYYY "-" MM "-" DD " " Www
			      (" " hh ":" mm))
			calc-display-working-message t)))
 '(org-completion-use-ido t)
 '(org-highlight-latex-and-related (quote (latex)))
 '(org-latex-default-class "book")
 '(package-selected-packages
   (quote
    (company jdee tide groovy-mode markdown-mode json-mode minimap sublimity tabbar ess htmlize web-mode powerline color-theme-solarized ac-octave ac-math ac-html-csswatcher ac-html-bootstrap ac-html ac-js2 ac-ispell auto-complete yasnippet stylus-mode jade-mode auctex kanban smooth-scrolling smartparens edit-server buffer-move ace-jump-mode)))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:underline "#b58900"))))
 '(flyspell-incorrect ((t (:foreground "#dc322f" :underline t :weight normal))))
 '(font-latex-italic-face ((t (:foreground "blue violet"))))
 '(font-latex-math-face ((t (:foreground "DeepSkyBlue3"))))
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#6c71c4" :weight bold :family "monospace"))) t)
 '(font-lock-builtin-face ((t (:foreground "dark goldenrod" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "SpringGreen4" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-comment-face ((t (:foreground "SpringGreen4" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-constant-face ((t (:foreground "color-73"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#586e75" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-doc-string-face ((t (:foreground "#586e75" :inverse-video nil :underline nil :slant normal :weight normal))) t)
 '(font-lock-function-name-face ((t (:foreground "yellow3" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "cyan3" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-string-face ((t (:foreground "color-34"))))
 '(font-lock-type-face ((t (:foreground "maroon" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "dodger blue" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(org-latex-and-related ((t (:foreground "color-59"))))
 '(sp-pair-overlay-face ((t (:background "brightblack" :foreground "white"))))
 '(sp-show-pair-enclosing ((t (:background "brightblack" :foreground "white"))))
 '(sp-show-pair-match-face ((t (:background "dark cyan" :foreground "white"))))
 '(sp-show-pair-mismatch-face ((t (:background "dark red" :foreground "white")))))
(put 'narrow-to-region 'disabled nil)
