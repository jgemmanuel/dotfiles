
;; org-mode templates
(add-hook 'find-file-hooks 'load-template)
(defun load-template ()
  (interactive)
  (when (and
         ;; (string-match "\\.org$" (buffer-file-name))
         (string-match "notes.org$" (buffer-file-name))
         (eq 1 (point-max)))
    (insert-file "~/.emacs.d/templates/notes.org")))
