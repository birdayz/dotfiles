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

;; packages
(straight-use-package 'evil)
(straight-use-package 'k8s-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ivy)
(straight-use-package 'go-mode)
(straight-use-package 'godoctor)
(straight-use-package 'go-complete)
(straight-use-package 'company)
(straight-use-package 'magit)
(straight-use-package 'flycheck)
(straight-use-package 'gradle-mode)
(straight-use-package 'groovy-mode)
(straight-use-package 'lsp-java)
(straight-use-package 'lsp-ui)
(straight-use-package 'cyberpunk-theme)
(straight-use-package 'smex)
(straight-use-package 'centaur-tabs)
(straight-use-package 'minimap)
(straight-use-package 'zoom)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'projectile)
(straight-use-package 'recentf)
(straight-use-package 'fzf)
(straight-use-package 'evil-magit)
(straight-use-package 'dashboard)
(straight-use-package 'company-box)
(straight-use-package 'company-posframe)
(straight-use-package 'monokai-theme)
(straight-use-package 'doom-modeline)
(straight-use-package 'yascroll)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'ace-window)
(straight-use-package 'doom-themes)

;; hotkeys
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "<f1>") 'counsel-find-file)
(global-set-key (kbd "<f2>") 'counsel-recentf)
(global-set-key (kbd "<f3>") 'fzf-git)
(global-set-key (kbd "<f4>") 'counsel-imenu)
(global-set-key (kbd "<f5>") 'counsel-rg)
(global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
(global-set-key (kbd "<f7>") 'counsel-bookmark)
(global-set-key (kbd "<f9>") 'text-scale-adjust)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; general
(set-frame-font "Source Code Pro 10" nil t)
(global-yascroll-bar-mode 1)
(require 'doom-modeline)
(doom-modeline-mode 1)

;; tuning
(setq gc-cons-threshold 1000000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.100)
(setq lsp-prefer-capf t)
(setq lsp-idle-delay 0.0500)

;; other
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "m" 'ace-window)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
(require 'dashboard)
(dashboard-setup-startup-hook)
(require 'evil-magit)
(ivy-mode 1)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(require 'lsp-mode)
(require 'go-complete)
(require 'company-posframe)
(company-posframe-mode 1)
(add-hook 'completion-at-point-functions 'go-complete-at-point)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; dashboard
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(add-hook 'before-save-hook #'lsp-format-buffer)
(add-hook 'before-save-hook #'lsp-organize-imports)

(setq org-agenda-files '("~/Documents/agenda/"))
(ace-window-display-mode 1)
(toggle-scroll-bar -1)
(load-theme 'doom-one t)
