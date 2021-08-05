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
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'dashboard)
(straight-use-package 'company-box)
(straight-use-package 'powerline)

;(require 'dashboard)
(dashboard-setup-startup-hook)


;; Enable evil
(evil-mode 1)
(ivy-prescient-mode 1)
(ivy-mode 1)

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
(global-set-key (kbd "<f3>") 'counsel-fzf)
;(global-set-key (kbd "<tab>") 'ace-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'dracula t)
(selectrum-mode +1)

(savehist-mode 1)


(define-key evil-normal-state-map "m" 'ace-window)

(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

(require 'powerline)
(powerline-default-theme)

(straight-use-package 'evil-collection)
(evil-collection-init)
