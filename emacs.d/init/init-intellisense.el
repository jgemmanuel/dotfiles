
;; Configure auto-complete and yasnippet
;; Credit: https://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
;; NOTE: auto-complete mode should be loaded after yasnippet so that they can
;; work together
(yas-global-mode 1)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; Turn on Semantic
(semantic-mode 1)

;; Define a function which adds semantic as a suggestion backend to auto-complete; hook this to whatever mode
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-source 'ac-source-semantic))

;; Turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

;; Iedit-mode
(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(global-set-key (kbd "C-;") 'iedit-dwim)
