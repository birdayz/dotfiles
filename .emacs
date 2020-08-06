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
(straight-use-package 'imenu-list)
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(straight-use-package 'company-prescient)
(straight-use-package 'expand-region)
(straight-use-package 'alect-themes)
(straight-use-package 'kaolin-themes)
(straight-use-package 'calc)
(straight-use-package 'mermaid-mode)
(straight-use-package 'format-all)
(straight-use-package 'protobuf-mode)
(straight-use-package 'json-mode)
(straight-use-package 'dap-mode)
(straight-use-package 'use-package)
(straight-use-package
 '(emacs-livedown :type git :host github :repo "shime/emacs-livedown"
            ))
;; (straight-use-package 'all-the-icons)
;; (use-package all-the-icons-ivy-rich
;;   :ensure t
;;   :straight t
;;   :init (all-the-icons-ivy-rich-mode 1))

;; (use-package ivy-rich
;;   :ensure t
;;   :straight t
;;   :init (ivy-rich-mode 1))



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
(global-set-key (kbd "<f2>") 'counsel-buffer-or-recentf)
(global-set-key (kbd "<f3>") 'fzf-git)
(global-set-key (kbd "<f4>") 'lsp-ui-imenu)
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
(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 10000)
(ace-window-display-mode 1)
(toggle-scroll-bar -1)
(load-theme 'doom-one t)
(global-linum-mode 1)
(global-hl-line-mode 1)
(add-hook 'go-mode-hook #'lsp)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2b303b" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#c0c5ce"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" default))
 '(fci-rule-color "#65737E")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#D08770"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#4f5b66"))
 '(magit-diff-use-overlays nil)
 '(objed-cursor-color "#BF616A")
 '(org-agenda-files nil)
 '(pdf-view-midnight-colors (cons "#c0c5ce" "#2b303b"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(rustic-ansi-faces
   ["#2b303b" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#c0c5ce"])
 '(vc-annotate-background "#2b303b")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3BE8C")
    (cons 40 "#bbbe86")
    (cons 60 "#d3be80")
    (cons 80 "#ECBE7B")
    (cons 100 "#e2ab77")
    (cons 120 "#d99973")
    (cons 140 "#D08770")
    (cons 160 "#cc8294")
    (cons 180 "#c97db8")
    (cons 200 "#c678dd")
    (cons 220 "#c370b6")
    (cons 240 "#c16890")
    (cons 260 "#BF616A")
    (cons 280 "#a35f69")
    (cons 300 "#875e68")
    (cons 320 "#6b5c67")
    (cons 340 "#65737E")
    (cons 360 "#65737E")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(add-hook 'java-mode-hook #'lsp)

(setq lsp-java-format-settings-url "https://github.com/google/styleguide/blob/gh-pages/eclipse-java-google-style.xml")
(setq lsp-vetur-server-command "/usr/bin/vls")

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))
