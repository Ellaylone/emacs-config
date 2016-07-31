;;; better-defaults.el --- Better defaults for emacs
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Better defaults for Emacs

;;; Code:

;;; better defaults
(defvar package-manager-list
  '(
    magit
    anzu
    auto-complete
    autopair
    flycheck
    yasnippet
    ace-window
    ws-butler
    smartparens
    projectile
    undo-tree
    clean-aindent-mode
    volatile-highlights
    iedit
    comment-dwim-2
    duplicate-thing
    ))

(package-manager-install)

(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (try-require "uniquify")
  (setq uniquify-buffer-name-style 'forward)

  (try-require"saveplace")
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

;; Package: anzu
(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; Package: duplicate-thing
(try-require "duplicate-thing")
(global-set-key (kbd "M-c") 'duplicate-thing)

;; Package: iedit
(try-require "iedit")
(global-set-key (kbd "C-;") 'iedit-mode)

;; Package: ace-window on M-p
(try-require "ace-window")
(global-set-key (kbd "M-p") 'ace-window)

;; Package: clean-aindent-mode
(try-require "clean-aindent-mode")
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: globally enable linum-mode
(global-linum-mode t)

;; Package: global yasnippet
(try-require "yasnippet")
(yas-global-mode 1)

;; Package: keybind for whitespace-mode
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Package: show trailing whitespaces
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Package: auto-complete
(ac-config-default)

;; TODO check
;; Package: comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Package: volatile-highlights
(try-require "volatile-highlights")
(volatile-highlights-mode t)

;; Package: autoindent on ret
(global-set-key (kbd "RET") 'newline-and-indent)

;; Package: ws-butler
(try-require "ws-butler")
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: smartparens
(try-require "smartparens-config")
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: undo-tree
(global-undo-tree-mode)

;; Package: projejctile
(try-require "projectile")
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(provide 'better-defaults)
;;; better-defaults.el ends here
