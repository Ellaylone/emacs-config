;;; better-defaults.el --- Better defaults for emacs
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Better defaults for Emacs

;;; Code:

;;; better defaults
(defvar ido-enable-flex-matching nil)
(defvar x-select-enable-cliboard nil)
(defvar x-select-enable-primary nil)
(defvar apropos-do-all nil)
(defvar ediff-window-setup-function nil)

(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'saveplace)
  (setq-default save-place t)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq x-select-enable-cliboard t
	x-select-enable-primary t
	save-interprogram-paste-before-kill t
	apropos-do-all t
	mouse-yank-at-point t
	require-final-newline t
	visible-bell t
	load-prefer-newer t
	ediff-window-setup-function 'ediff-setup-windows-plain
	save-place-file (concat user-emacs-directory "places")
	backup-directory-alist `(("." . ,(concat user-emacs-directory
						 "backups")))))
;;; better defaults

;; ace-window on M-p
(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

;; globally enable linum-mode
(global-linum-mode t)

;; global yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; keybind for whitespace-mode
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show trailing whitespaces
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; auto-complete
(ac-config-default)

;; autoindent on ret
(global-set-key (kbd "RET") 'newline-and-indent)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)
;; (smartparens-strict-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(provide 'better-defaults)
;;; better-defaults.el ends here
