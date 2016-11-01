;;; react.el --- Package Manager:
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Config for react development

;;; Code:

(setq package-manager-list
      '(
        jsx-mode
        js2-mode
        js2-refactor
        exec-path-from-shell
        ))

(package-manager-install)

;; Package: js2-mode
(try-require "js2-mode")

;; Package: js2-refactor
(try-require "js2-refactor")
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; js2-imenu-extras-setup
(add-hook 'js2-mode-hook #'js2-imenu-extras-setup)

;; ;; Package: javascript-eslint
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint)))
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'js2-mode)

;; ;; Package jsx-mode
;; (try-require "jsx-mode")
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;; (autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; ;; React support in js2-imenu-extras
;; (eval-after-load 'js2-mode
;;   '(progn
;;      (try-require "js2-imenu-extras")

;;      ;; The code to record the class is identical to that for
;;      ;; Backbone so we just make an alias
;;      (defalias 'js2-imenu-record-react-class
;;        'js2-imenu-record-backbone-extend)

;;      (unless (loop for entry in js2-imenu-extension-styles
;;                    thereis (eq (plist-get entry :framework) 'react))
;;        (push '(:framework react
;;                :call-re "\\_<React\\.createClass\\s-*("
;;                :recorder js2-imenu-record-react-class)
;;              js2-imenu-extension-styles))

;;      (add-to-list 'js2-imenu-available-frameworks 'react)
;;      (add-to-list 'js2-imenu-enabled-frameworks 'react)))

;; ;; <> as delimeters in js2-mode
;; (defun modify-syntax-table-for-jsx ()
;;   (modify-syntax-entry ?< "(>")
;;   (modify-syntax-entry ?> ")<"))

;; (add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)

;; (provide 'react)
;;; react ends here


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))

(provide 'react)
;;; react ends here
