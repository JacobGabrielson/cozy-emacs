;;; -*- lexical-binding: t -*-

;;; Everything that depends on stuff that isn't just built in to
;;; Emacs.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-pinned-packages '(org . "org") t)
(add-to-list 'package-pinned-packages '(org-plus-contrib . "org") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package ripgrep)

(use-package excorporate)

(use-package systemd)

(use-package dockerfile-mode)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package wgrep :init (require 'wgrep))

(use-package ggtags)

(use-package projectile
  :config
  (progn
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-x c") 'projectile-compile-project)
    (projectile-mode +1)))

(use-package markdown-mode)

;; C/C++

(when window-system
  (use-package git-gutter-fringe
    :config
    (progn
      (dolist (p '((git-gutter:added    . "#0c0")
		   (git-gutter:deleted  . "#c00")
		   (git-gutter:modified . "#c0c")))
	(set-face-foreground (car p) (cdr p))
	(set-face-background (car p) (cdr p)))
      (global-git-gutter-mode t))))

;; (use-package smex
;;   :config (progn)
;;   (smex-initialize)
;;   (global-set-key (kbd "M-x") 'smex)
;;   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;   ;; This is your old M-x.
;;   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;; Setting this in custom.el doesn't work, because that gets loaded
;; _after_ comint gets loaded
(progn
  (setq password-word-equivalents
        '("PIN" "password" "passcode" "passphrase" "pass phrase"))

  (custom-reevaluate-setting 'comint-password-prompt-regexp))


(use-package org)

(org-babel-do-load-languages 'org-babel-load-languages
			     '(
			       (shell . t)
			       )
			     )

(use-package flycheck
  :ensure t)

;; Note: make sure shellcheck pkg is installed w/ apt or whatnot
(add-hook 'sh-mode-hook 'flycheck-mode)

(use-package highlight-indent-guides)

(use-package yaml-mode)

;; (use-package helm
;;   :ensure t
;;   :config
;;   (progn
;;     (helm-mode 1)
;;     (global-set-key (kbd "C-x C-f") 'helm-find-files)
;;     (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package flx)

(use-package counsel
  :config
  (progn
    ;; from http://oremacs.com/swiper/#installation
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-plus)))
    
    (counsel-mode 1)
    (global-set-key (kbd "C-s") 'swiper-isearch)
    (global-set-key (kbd "C-x b") 'ivy-switch-buffer)

    (global-set-key (kbd "C-c c") 'counsel-compile)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c L") 'counsel-git-log)
    (global-set-key (kbd "C-c k") 'counsel-rg)
    (global-set-key (kbd "C-c m") 'counsel-linux-app)
    (global-set-key (kbd "C-c n") 'counsel-fzf)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-c J") 'counsel-file-jump)
    (setq projectile-completion-system 'ivy)
    ))

