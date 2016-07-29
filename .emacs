;;; .emacs --- Config for emacs
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Simple Emacs config for personal use

;;; Code:
;; Define path to modules
(message "Emacs init")

;; Variables for path to modules & elpa
(defvar modules-path nil
  "Defines path to modules directory.")
(setq modules-path (concat user-emacs-directory
        (convert-standard-filename "modules")))
(defvar elpa-path nil
  "Defines path to elpa directory.")
(setq elpa-path (concat user-emacs-directory
        (convert-standard-filename "elpa")))

;; Add elpa-path & modules-path to load-path
(let ((default-directory elpa-path))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory modules-path))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(defvar missing-packages-list
  '())
(defun try-require (feature)
  "Attempt to load a library or module.
Return true if the library given as argument is successfully loaded.
If not, instead of an error, just add the package to a list of missing packages.
FEATURE - feature to require"
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

;; Loading package manager
(load "package-manager")
(load "better-defaults")

;; (package-manager-install)

(provide 'emacs)
;;; .emacs ends here
