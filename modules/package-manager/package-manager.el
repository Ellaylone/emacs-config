;;; package-manager.el --- Package Manager:
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Simple package manager for Emacs
;; Loads missing packages on .emacs load
;;;PACKAGE-LIST

;;; Code:
;; list repos
(message "Init package-manager.el")
(require 'package)
(package-initialize)

(defvar package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/"))
  "List of package archives.")


;; Define package-manager-list
(defvar package-manager-list nil
  "Lists packages to be installed on load.")

;; Define package-manager-install
(defun package-manager-install ()
  "Install packages from package-manager-list.
Uses lift of packages stored in variable package-manager-list"
  (interactive)
  (defun package-manager-list-installed-p ()
    "Loop through package-manager-list and look for not installed packages."
    (loop for p in package-manager-list
          when (not (package-installed-p p)) do (return nil)
          finally (return t)))

  (unless (package-manager-list-installed-p)
    (message "Check for new packages: init")
    (package-refresh-contents)
    (message "%s" "Check for new packages: done")
    (dolist (p package-manager-list)
      (when (not (package-installed-p p))
        (package-install p))))
  (message ("Packages installed"))
  )

(provide 'package-manager)
;;; package-manager.el ends here
