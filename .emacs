;;; .emacs --- Config for emacs
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Simple Emacs config for personal use

;;; Code:

;; Define path to modules
(defvar modules-path)
(setq modules-path (concat user-emacs-directory
        (convert-standard-filename "modules/")))

;; Add modules-path to load-path
(let ((default-directory modules-path))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; TODO macro message for failed require


;; Loading package manager
(load-library "package-manager")

(provide 'emacs)
;;; .emacs ends here
