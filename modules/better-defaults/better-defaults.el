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

  (try-require "saveplace")
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
;; adds editorconfig support to emacs

;; https://github.com/editorconfig/editorconfig-emacs
(try-require "editorconfig")
(editorconfig-mode 1)

;; Package: anzu
;; displays current match / total matches to search modes
;; https://github.com/syohex/emacs-anzu
(global-anzu-mode 1)
;; enable anzu replace commands by default
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; Package: duplicate-thing
;; duplicate line or region
;; https://github.com/ongaeshi/duplicate-thing
(try-require "duplicate-thing")
(global-set-key (kbd "M-c") 'duplicate-thing)

;; Package: iedit
;; multicursor for editing
;; https://github.com/victorhge/iedit
(try-require "iedit")
(global-set-key (kbd "C-;") 'iedit-mode)

;; Package: ace-window on M-p
;; better way to switch between windows
;; https://github.com/abo-abo/ace-window
;; TODO: hydra with ace-window actions ?
(try-require "ace-window")
(global-set-key (kbd "M-p") 'ace-window)

;; Package: clean-aindent-mode
;; better auto-indent and unindent
;; https://github.com/pmarinov/clean-aindent-mode
(try-require "clean-aindent-mode")
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: globally enable linum-mode
;; always display line numbers
;; TODO hydra for enhanced goto line
;; ace-jump-mode allows to jump lines in viewport
;; TODO disable by default once goto & jump lines are usable
(global-linum-mode t)

;; Package: global yasnippet
;; template system
;; https://github.com/joaotavora/yasnippet

(try-require "yasnippet")
(yas-global-mode 1)

;; Package: keybind for whitespace-mode
;; render spaces, tabs, newlines with a visible glyph
;; http://ergoemacs.org/emacs/whitespace-mode.html
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Package: show trailing whitespaces
;; show line trailing whitespaces
;; http://ergoemacs.org/emacs/whitespace-mode.html
;; TODO replace with https://www.emacswiki.org/emacs/EightyColumnRule
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Package: auto-complete
;; auto complete dropdown
;; https://github.com/auto-complete/auto-complete
;; TODO revise
(ac-config-default)

;; Package: comment-dwim-2
;; better comment features
;; https://github.com/remyferre/comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Package: volatile-highlights
;; visual feedback with actions like undo, yank etc
;; https://github.com/k-talo/volatile-highlights.el
(try-require "volatile-highlights")
(volatile-highlights-mode t)

;; autoindent on ret
;; TODO remove as it's not really used
(global-set-key (kbd "RET") 'newline-and-indent)

;; Package: ws-butler
;; trim whitespaces when saving
;; https://github.com/lewang/ws-butler
(try-require "ws-butler")
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: smartparens
;; better pairs
;; https://github.com/Fuco1/smartparens
(try-require "smartparens-config")
(setq sp-base-key-bindings 'paredit)
;; don't insert pair when cursor followed by word
(sp-pair "(" nil :unless '(sp-point-before-word-p))
(sp-pair "[" nil :unless '(sp-point-before-word-p))
(sp-pair "{" nil :unless '(sp-point-before-word-p))
(sp-use-paredit-bindings)
(show-smartparens-global-mode 1)
(smartparens-global-mode 1)

;; Package: undo-tree
;; visual for undo history with branches and stuff
;; https://www.emacswiki.org/emacs/UndoTree
(global-undo-tree-mode)
(setq undo-tree-visualizer-diff 1)
(setq undo-tree-visualizer-timestamps 1)
(setq undo-tree-auto-save-history 1)
;; make dir to save undo tree history
(setq undo-tree-history-dir (let ((dir (concat user-emacs-directory
                                               "undo-tree-history/")))
                              (make-directory dir :parents)
                              dir))
(setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-dir)))

;; Package: projejctile
;; better project navigation
;; https://github.com/bbatsov/projectile
(try-require "projectile")
(projectile-global-mode)

;; Package zygospore
;; toggle C-x 1
;; https://github.com/LouisKottmann/zygospore.el
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Package helm
;; TODO
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
;; tree project eplorer
;; TODO cannot index huge projects, look for replacement
;; https://github.com/sabof/project-explorer
(try-require "project-explorer")

;; Package drag-stuff
;; drag lines and regions
;; https://github.com/rejeep/drag-stuff.el
(try-require "drag-stuff")
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Package browse-kill-ring
;; show kill ring with ability to select what to yank
;; https://github.com/browse-kill-ring/browse-kill-ring
(try-require "browse-kill-ring")
(global-set-key (kbd "C-y") 'browse-kill-ring)

;; Package markdown-mode
;; md support and visualisation
;; https://github.com/defunkt/markdown-mode
(try-require "markdown-mode")

;; Package god-mode
;; https://github.com/chrisdone/god-mode
(try-require "god-mode")
(global-set-key (kbd "<escape>") 'god-mode-all)

;; Package hydra
;; tie commands in bindings
;; https://github.com/abo-abo/hydra
(try-require "hydra")

;; Package ace-popup-menu
;; TODO
(try-require "ace-popup-menu")
(ace-popup-menu-mode 1)

;; Package goto-last-change
;; returns cursor to last edit
;; https://github.com/camdez/goto-last-change.el
(try-require "goto-last-change")
(global-set-key (kbd "C-x C-/") 'goto-last-change)

;; Package ace-jump-mode
;; better cursor movement in viewport
;; https://github.com/winterTTr/ace-jump-mode
(try-require "ace-jump-mode")
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Package workgroups2
;; session manager
;; https://github.com/pashinin/workgroups2
(try-require "workgroups2")
(workgroups-mode 1)

(provide 'better-defaults)
;;; better-defaults.el ends here
