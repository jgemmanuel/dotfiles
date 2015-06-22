;; Enable the mode globally
(smartparens-global-mode t)

;; Highlights matching pairs
(show-smartparens-global-mode t)
(setq sp-show-pair-delay 0)

;; Markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-pair "`" "`"))

;; Org- and LaTeX-mode
(sp-with-modes '(org-mode LaTeX-mode)
  (sp-local-pair "$" "$"))
(sp-with-modes 'org-mode (sp-local-pair "=" "="))

;; Keybindings
(define-key sp-keymap (kbd "C-` <right>") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-` <left>") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-` <down>") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-` <up>") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-` C-<right>") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-` C-<left>") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-` C-<up>") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-` C-<down>") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-` C-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-` M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "C-` C-q") 'sp-unwrap-sexp) ;unwraps the next pair in front
(define-key sp-keymap (kbd "C-` C-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-` S-<down>") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-` S-<right>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-` S-<left>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-` S-<up>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-` s") 'sp-split-sexp)
(define-key sp-keymap (kbd "C-` S-s") 'sp-join-sexp)

(define-key sp-keymap (kbd "C-` r") 'sp-rewrap-sexp) ;must be inside the pair

(define-key sp-keymap (kbd "C-` M-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-` M-<up>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-` M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-` M-<down>") 'sp-backward-barf-sexp)

;; ;; movement
;; sp-forward-sexp (&optional arg)                 ;; C-M-f
;; sp-backward-sexp (&optional arg)                ;; C-M-b
;; sp-down-sexp (&optional arg)                    ;; C-M-d
;; sp-backward-down-sexp (&optional arg)           ;; C-M-a
;; sp-up-sexp (&optional arg)                      ;; C-M-e
;; sp-backward-up-sexp (&optional arg)             ;; C-M-u
;; sp-next-sexp (&optional arg)                    ;; C-M-n
;; sp-previous-sexp (&optional arg)                ;; C-M-p
;; sp-beginning-of-sexp (&optional arg)            ;; C-S-d
;; sp-end-of-sexp (&optional arg)                  ;; C-S-a
;; sp-beginning-of-next-sexp (&optional arg)       ;; none
;; sp-beginning-of-previous-sexp (&optional arg)   ;; none
;; sp-end-of-next-sexp (&optional arg)             ;; none
;; sp-end-of-previous-sexp (&optional arg)         ;; none

;; ;; manipulation
;; sp-kill-sexp (&optional arg)                        ;; C-M-k
;; sp-backward-kill-sexp (&optional arg)               ;; C-- C-M-k

;; sp-copy-sexp (&optional arg)                        ;; C-M-w
;; sp-backward-copy-sexp (&optional arg)               ;; C-- C-M-w

;; sp-unwrap-sexp (&optional arg)                      ;; M-<delete>
;; sp-backward-unwrap-sexp (&optional arg)             ;; M-<backspace>

;; sp-transpose-sexp                                   ;; C-M-t

;; sp-splice-sexp (&optional arg)                      ;; M-D
;; sp-splice-sexp-killing-forward (&optional arg)      ;; C-M-<delete>
;; sp-splice-sexp-killing-backward (&optional arg)     ;; C-M-<backspace>
;; sp-splice-sexp-killing-around (&optional arg)       ;; C-S-<backspace>

;; sp-convolute-sexp (&optional arg)                   ;; none
;; sp-absorb-sexp (&optional arg)                      ;; none
;; sp-emit-sexp (&optional arg)                        ;; none
;; sp-extract-before-sexp (&optional arg)              ;; none
;; sp-extract-after-sexp (&optional arg)               ;; none

;; sp-split-sexp (arg)                                 ;; none
;; sp-join-sexp (&optional arg)                        ;; none

;; sp-rewrap-sexp (&optional arg)                      ;; none
;; sp-swap-enclosing-sexp (&optional arg)              ;; none

;; sp-forward-slurp-sexp (&optional arg)               ;; C-<right>
;; sp-forward-barf-sexp (&optional arg)                ;; C-<left>
;; sp-backward-slurp-sexp (&optional arg)              ;; C-M-<left>
;; sp-backward-barf-sexp (&optional arg)               ;; C-M-<right>

;; sp-add-to-next-sexp (&optional arg)                 ;; none
;; sp-add-to-previous-sexp (&optional arg)             ;; none

;; sp-select-next-thing (&optional arg)                ;; C-M-]
;; sp-select-previous-thing (&optional arg)            ;; C-[

;; sp-select-next-thing-exchange (&optional arg)       ;; C-]
;; sp-select-previous-thing-exchange (&optional arg)   ;; C-- C-]

;; Indentation after braces for certain modes
;; Credit: @wasamasa (http://emacs.stackexchange.com/a/3015)
(sp-local-pair 'c++-mode "{" nil :post-handlers '((custom-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'sh-mode "{" nil :post-handlers '((custom-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'java-mode "{" nil :post-handlers '((custom-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'js-mode "{" nil :post-handlers '((custom-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'latex-mode "{" nil :post-handlers '((custom-create-newline-and-enter-sexp "RET")))

(defun custom-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
