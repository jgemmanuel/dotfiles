;; Add Melpa repository
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Activate installed packages
(package-initialize)

;; Automate package installation
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every package not installed."
  (mapcar
   (lambda (package)
     (package-installed-p 'evil)
     (if (package-installed-p package)
	 package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package)
	 nil)))
   packages))
;; Update package list
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
;; Install missing packages
(ensure-package-installed 'smartparens 'org-plus-contrib 'auctex 'jade-mode 'stylus-mode 'smooth-scrolling 'ace-jump-mode)
;; (if (display-graphic-p)
;;     (progn
;;       ;; if graphic
;;       (your)
;;       (code))
;;   ;; else (optional)
;;   (your)
;;   (code))
(when (display-graphic-p)
  (ensure-package-installed 'color-theme-solarized))

;; ;; Activate installed packages
;; (package-initialize)
