;; -*- mode: emacs-lisp -*-
;; Simple init.el configuration

;; Hide menu, toolbar, and welcome message
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; Load smooth-scrolling
(add-to-list 'load-path "~/.emacs.d")
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; Load Org-mode
(add-to-list 'load-path "~/.emacs.d/org-8.2.5h/lisp")
(add-to-list 'load-path "~/.emacs.d/org-8.2.5h/contrib/lisp" t)

;; Buffer names management (cf `custom-set-variables')
(require 'uniquify)

;; Load package management related content
(load "~/.emacs.d/init-package")

;; Insert parentheses in pairs
(load "~/.emacs.d/init-smartparens")
;; (electric-pair-mode 1)

;; Load ace-jump-mode
;; "C-c SPC" --> ace-jump-word-mode
;; "C-u C-c SPC" --> ace-jump-char-mode
;; "C-u C-u C-c SPC" --> ace-jump-line-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Set width of screen for the purpose of word-wrapping (enable `auto-fill-mode')
(setq-default fill-column 78)

;; Auto indentation (Not supported by some modes, eg f90-mode)
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

;; Toggle `linum-mode' (cf `custom-set-variables')
(global-set-key (kbd "<f7>") 'linum-mode)

;; org-mode
(load "~/.emacs.d/init-org")
;; shell-mode (fix regarding garbled characters)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Autolad Octave mode for *.m files
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; Jade- and Stylus-modes
(add-to-list 'load-path "~/.emacs.d/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

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


;;;; Putty + SHIFT *
;; (define-key input-decode-map "ESC-[ a" [S-up])


;;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(linum-format (quote "%3d"))
 '(org-calc-default-modes (quote (calc-internal-prec 20 calc-float-format (float 8) calc-angle-mode rad calc-prefer-frac nil calc-symbolic-mode nil calc-date-format (YYYY "-" MM "-" DD " " Www (" " hh ":" mm)) calc-display-working-message t)))
 '(org-completion-use-ido t)
 '(org-highlight-latex-and-related (quote (latex)))
 '(org-html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
             styles: {			// increase margins
	         \".MathJax_Display\": {
                     \"text-align\": \"center\",
                     margin: \"1.75em 0em\"
                 }
	     }
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        },
        TeX: {
             Macros: {
                 cov: \"\\\\mathrm{cov}\",
                 corr: \"\\\\mathrm{corr}\",
                 brac: [\"{\\\\left( #1\\\\right)}\", 1],
                 Brac: [\"{\\\\left[ #1\\\\right]}\", 1],
                 Brace: [\"{\\\\left\\\\{ #1\\\\right\\\\}}\", 1],
                 abs: [\"{\\\\left\\\\lvert #1\\\\right\\\\rvert}\", 1],
                 Expect: [\"{\\\\mathrm{E}\\\\Brac{#1}}\", 1],
                 pD: [\"{\\\\frac{\\\\partial #1}{\\\\partial #2}}\", 2],
                 coloneqq: \"{\\\\mathrel{\\\\vcenter{:}}=}\",
                 intercal: \"{\\\\top}\",
                 bm: [\"{\\\\bf #1}\", 1]
             }
        }
    });
/*]]>*///-->
</script>")
 '(org-latex-default-class "book")
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
 '(font-lock-variable-name-face ((t (:foreground "blue"))))
 '(org-latex-and-related ((t (:foreground "color-59"))))
 '(sp-show-pair-enclosing ((t (:background "brightblack" :foreground "white"))))
 '(sp-show-pair-match-face ((t (:background "dark cyan" :foreground "white"))))
 '(sp-show-pair-mismatch-face ((t (:background "dark red" :foreground "white")))))
(put 'narrow-to-region 'disabled nil)
