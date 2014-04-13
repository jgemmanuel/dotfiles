;;;; Org-mode
;;; Acknowledgement: @Yoo (http://stackoverflow.com/q/2736087)

(eval-after-load "org"
  '(progn
     ;; (define-key org-mode-map (kbd "C-c M-<right>") 'org-table-insert-column)
     ;; (define-key org-mode-map (kbd "C-c M-<left>") 'org-table-delete-column)
     ;; (define-key org-mode-map (kbd "C-c M-<down>") 'org-table-insert-row)
     ;; (define-key org-mode-map (kbd "C-c M-<up>") 'org-table-kill-row)

     (require 'latex)
     (define-key org-mode-map (kbd "C-c C-t") 'LaTeX-environment)

     ;; Org-mode smart quoting and exporting
     ;; (require 'ox)
     (require 'ox-latex)
     (setq-default org-export-with-smart-quotes t)
     (add-to-list 'org-export-smart-quotes-alist
		  '("en-GB"
		    (opening-double-quote :utf-8 "“" :html "<q>" :latex "\\textquote{" :texinfo "``")
		    (closing-double-quote :utf-8 "”" :html "</q>" :latex "}" :texinfo "''")
		    (opening-single-quote :utf-8 "‘" :html "<q>" :latex "\\textquote{" :texinfo "`")
		    (closing-single-quote :utf-8 "’" :html "</q>" :latex "}" :texinfo "'")
		    (apostrophe :utf-8 "’" :html "&rsquo;")))

     ;; Org-mode latex exports
     (setq org-latex-listings 'listings)
     (unless (boundp 'org-latex-classes)
       (setq org-latex-classes nil))
     (add-to-list 'org-latex-classes
		  '("book"
		    "\\RequirePackage[l2tabu, orthodox]{nag}
\\documentclass{book}
\\usepackage[a4paper]{geometry}
\\usepackage{graphicx}
\\usepackage{mathptmx, tgtermes}
\\usepackage[T1]{fontenc}
\\usepackage{microtype}
\\usepackage{siunitx}
\\usepackage{booktabs}
\\usepackage[listings, mathtools, simpleheadings]{custom}
\\usepackage[british]{babel}
\\usepackage{csquotes}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]

                    [EXTRA]

\\usepackage[colorlinks=false, pdfborder={0 0 0}]{hyperref}
\\usepackage{cleveref}"
		    ("\\chapter{%s}" . "\\chapter*{%s}")
		    ("\\section{%s}" . "\\section*{%s}")
		    ("\\subsection{%s}" . "\\subsection*{%s}")
		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		    ("\\paragraph{%s}" . "\\paragraph*{%s}")))

     (defun my-org-mode-hook ()
       ;; The following two lines of code is run from the mode hook.
       ;; These are for buffer-specific things.
       ;; In this setup, you want to enable flyspell-mode
       ;; as well as auto-fill-mode for every org buffer.
       (flyspell-mode 1)
       (auto-fill-mode 1))
     (add-hook 'org-mode-hook 'my-org-mode-hook)))

;; ;; Org-mode source code execution
(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (R . t)
			       (python . t)
			       (octave . t)
			       (latex . t)))
;; Fontify source code blocks
(setq org-src-fontify-natively t)
