;; Add Melpa repository
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
(ensure-package-installed 'smartparens)

;; ;; Activate installed packages
;; (package-initialize)
