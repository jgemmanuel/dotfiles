
(add-hook 'find-file-hooks 'load-templates)
(defun load-templates ()
  (interactive)
  ;; org-mode templates
  (when (and
         (string-match "notes.org$" (buffer-file-name))
         (eq 1 (point-max)))
    (insert-file "~/.emacs.d/templates/notes.org"))
  ;; test template
  (when (and
         (string-match "\\.test$" (buffer-file-name))
         (eq 1 (point-max)))
    (insert-file "~/.emacs.d/templates/test.test")))
