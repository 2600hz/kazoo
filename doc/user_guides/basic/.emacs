;; emacs config needed to build PDFs
(setq package-user-dir "./.emacs.d")
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (package-install package)
       package))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir) (package-refresh-contents))

;; activate installed packages
(package-initialize)

(ensure-package-installed 'org) ;  --> (nil nil) if iedit and magit are already installed
