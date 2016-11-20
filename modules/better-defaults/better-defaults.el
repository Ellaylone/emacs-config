;;; better-defaults.el --- Better defaults for emacs
;; Author: Ellaylone <heamik91@yandex.ru>
;; Version: 0.0.1

;;; Commentary:
;; Better defaults for Emacs

;;; Code:
(setq package-manager-list
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
        editorconfig
        zygospore
        helm
        helm-swoop
        project-explorer
        browse-kill-ring
        drag-stuff
        markdown-mode
        god-mode
        ace-popup-menu
        hydra
        goto-last-change
        ace-jump-mode
        workgroups2
        ))

(package-manager-install)

;;; better defaults
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

;; Package: editorconfig
(try-require "editorconfig")
(editorconfig-mode 1)

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

;; Package helm
(try-require "helm")
(try-require "helm-swoop")
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
(setq helm-multi-swoop-edit-save t) ;; ?
(setq helm-swoop-split-with-multiple-windows t)
(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-speed-or-color nil)
(setq helm-swoop-move-to-line-cycle t)
(setq helm-swoop-use-line-number-face t)
(setq helm-swoop-use-fuzzy-match t)

;; Package project-explorer
(try-require "project-explorer")

;; Package drag-stuff
(try-require "drag-stuff")
(drag-stuff-mode t)

;; Call whitespace-cleanup before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Package browse-kill-ring
(try-require "browse-kill-ring")
(global-set-key (kbd "C-y") 'browse-kill-ring)

;; Package markdown-mode
(try-require "markdown-mode")

;; Package god-mode
(try-require "god-mode")
(global-set-key (kbd "<escape>") 'god-mode-all)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

;; Package hydra
(try-require "hydra")

;; Package ace-popup-menu
(try-require "ace-popup-menu")
(ace-popup-menu-mode 1)

;; Package goto-last-change
(try-require "goto-last-change")
(global-set-key (kbd "C-x C-/") 'goto-last-change)

;; Package ace-jump-mode
(try-require "ace-jump-mode")
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Package workgroups2
(try-require "workgroups2")
(workgroups-mode 1)

(provide 'better-defaults)
;;; better-defaults.el ends here
