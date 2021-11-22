(setq evil-want-keybinding nil)
;; setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'evil)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'go-mode)
(straight-use-package 'company-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ace-window)
(straight-use-package 'magit)
(straight-use-package 'yaml-mode)
(straight-use-package 'dracula-theme)
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(straight-use-package 'company-prescient)
;;(straight-use-package 'selectrum)
;;(straight-use-package 'selectrum-prescient)
(straight-use-package 'dashboard)
(straight-use-package 'company-box)
;;(straight-use-package 'powerline)
(straight-use-package 'prism)
(straight-use-package 'undo-fu)
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'fzf)
(straight-use-package 'bazel)
(straight-use-package 'protobuf-mode)
(straight-use-package 'projectile)

;(require 'dashboard)
(dashboard-setup-startup-hook)


;; Enable evil
(evil-mode 1)
(ivy-mode 1)
(ivy-prescient-mode 1)

(setq vc-follow-symlinks t)

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; General settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

(prescient-persist-mode +1)


;; keybinds
(global-set-key (kbd "<f1>") 'counsel-find-file)
(global-set-key (kbd "<f2>") 'counsel-recentf)
(global-set-key (kbd "<f3>") 'fzf-git)
(global-set-key (kbd "<f4>") 'counsel-imenu)
(global-set-key (kbd "<f5>") 'counsel-rg)
(global-set-key (kbd "<f9>") 'text-scale-adjust)
;(global-set-key (kbd "<tab>") 'ace-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" default))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'dracula t)
;;(selectrum-mode +1)

(savehist-mode 1)


(define-key evil-normal-state-map "m" 'ace-window)

(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;;(require 'powerline)
;;(powerline-default-theme)

(straight-use-package 'evil-collection)
(evil-collection-init)
;(prism-mode 1)

;(global-undo-tree-mode -1)
(define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)


(doom-modeline-mode 1)
;;(setq lsp-enable-file-watchers nil)

;;(with-eval-after-load 'lsp-mode
;;  (add-to-list 'lsp-file-watch-ignored-directories "bazel-bin")
 ;; ;; or
;;  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'"))

  ;;(push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored-files) ; json



(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-bin$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-build$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-esp-iot$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-out$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-testlogs$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]app$")
  ;; or
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'"))
(setq lsp-file-watch-threshold 2000)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-file-watch-threshold 3000)

(setq backup-directory-alist `(("." . "~/.saves")))

(setq delete-old-versions t)
