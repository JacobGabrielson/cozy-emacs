;;; -*- lexical-binding: t -*-

;;; Everything that depends on stuff that isn't just built in to
;;; Emacs.

;; fyi

;; FYI M-x disable-theme
;;(load-theme 'dracula t)


;; Make sure everything is vanilla for a while
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

(use-package diminish)

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(use-package xclip
  :config
  ;; allow copy/paste from terminal on Mac OS X etc
  (xclip-mode 1))

(use-package ripgrep)

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package hcl-mode)
(pushnew '("\\.tf$" . hcl-mode) auto-mode-alist)

(use-package magit
  :bind
  ("C-x g" . magit-status)

  :config
  (setq magit-log-arguments '("-n256" "--graph" "--decorate" "--color")
        ;; Show diffs per word, looks nicer!
        magit-diff-refine-hunk t))

(use-package wgrep :init (require 'wgrep))

(use-package projectile
  :config
  (progn
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-x c") 'projectile-compile-project)
    (projectile-mode +1)))

(when window-system
  (use-package git-gutter-fringe
    :config
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
    (global-git-gutter-mode t)))

;; Setting this in custom.el doesn't work, because that gets loaded
;; _after_ comint gets loaded
(progn
  (setq password-word-equivalents
        '("PIN" "password" "passcode" "passphrase" "pass phrase"))

  (custom-reevaluate-setting 'comint-password-prompt-regexp))


(use-package org
  :config
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?Z)
  (setq org-image-actual-width nil)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-hierarchical-todo-statistics nil) ; recurse!
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-provide-todo-statistics t)
  (setq org-todo-keywords
	'((sequence "TODO" "|" "DONE" "DELEGATED" "CANCELED")))
  (setq org-adapt-indentation nil)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '(
				 (shell . t)
				 )))

(require 'ox-odt)
(setq org-odt-preferred-output-format "docx")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (gnuplot . t)))

(use-package sudo-edit :ensure t)

(use-package flycheck)

;; Note: make sure shellcheck pkg is installed w/ apt or whatnot
(add-hook 'sh-mode-hook 'flycheck-mode)

;; causes problems for large docs from oss projs (eg)
;;(add-hook 'markdown-mode-hook 'flycheck-mode)
;;(add-hook 'markdown-mode-hook 'flyspell-buffer)
;;(add-hook 'markdown-mode-hook 'flyspell-mode-on)
;;(remove-hook 'markdown-mode-hook 'flyspell-mode-on)


;;(use-package highlight-indent-guides
;;  :config
;;  (progn
;;    (add-hook 'prog-mode-hook 'custom-prog-modes-hook)
;;  ))

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
    ;; restore familiar behavior
    (setq ivy-magic-tilde nil)
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

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package indent-guide)

; setup Emacs path from our ~/.bashrc
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package highlight-indent-guides)

(setq highlight-indent-guides-method 'bitmap)

(add-hook 'prog-mode-hook 'custom-prog-modes-hook)

(defun custom-prog-modes-hook ()
  (highlight-indent-guides-mode 1)
  (setq display-line-numbers t)
  (display-fill-column-indicator-mode 1))



;;(use-package dirvish)
;;(dirvish-override-dired-mode)

(use-package pdf-tools)
(use-package vscode-icon)

;; https://github.com/Wilfred/deadgrep
(use-package deadgrep)
(use-package yasnippet)

;; for live stream/screencasts
;;(use-package command-log-mode)

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))
