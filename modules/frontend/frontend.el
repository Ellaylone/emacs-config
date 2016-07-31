;;; frontend.el --- Package Manager:
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Config for frontend development

;;; Code:

(setq package-manager-list
      '(
        web-mode
        pug-mode
        js2-mode
        js2-refactor
        web-beautify
        skewer-mode
        impatient-mode
        json-mode
        emmet-mode
        ))

(package-manager-install)

;; Package: web-mode
(try-require "web-mode")
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Package: pug-mode
(try-require "pug-mode")

;; Package: js2-mode
(try-require "js2-mode")

;; Package: js2-refactor
(try-require "js2-refactor")
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; Package web-beautify
(try-require "web-beautify")
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; Package skewer-mode
(try-require "skewer-mode")
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

;; Package impatient-mode
(try-require "impatient-mode")
(add-hook 'html-mode-hook 'impatient-mode)
(add-hook 'web-mode-hook 'impatient-mode)

;; Package javascript-eslint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; Package json-mode
(try-require "json-mode")

;; Package emmet-mode
(try-require "emmet-mode")
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(provide 'frontend)
;;; frontend ends here
