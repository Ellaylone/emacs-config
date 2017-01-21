;;; jde.el --- Package Manager:
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Config for java development

;;; Code:

(setq package-manager-list
      '(
        eclim
        ))

(package-manager-install)

;; Package: eclim
(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)
(custom-set-variables
 '(eclim-eclipse-dirs '("/Applications/eclipse")))

(provide 'jde)
;;; jde ends here
