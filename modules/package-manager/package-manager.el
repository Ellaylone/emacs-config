;;; package-manager.el --- Package Manager:
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Simple package manager for Emacs
;; Loads missing packages on .emacs load
;;;PACKAGE-LIST

;;; Code:
;; list repos
(defvar package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/"))
  "List of package archives.")


;; Define package-manager-list
(defvar package-manager-list nil
  "Lists packages to be installed on load.")

;; Define package-manager-install
(defun package-manager-install ()
  "Install packages on load.
Uses lift of packages stored in variable package-manager-list"
  (interactive)
  (message "asdasd"))

(message "test")

(provide 'package-manager)
;;; package-manager.el ends here
