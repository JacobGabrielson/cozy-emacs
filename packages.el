;;; -*- lexical-binding: t -*-

;;; Everything that depends on stuff that isn't just built in to
;;; Emacs.

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(org . "org") t)
(add-to-list 'package-pinned-packages '(org-plus-contrib . "org") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package go-mode)

(use-package excorporate)

(use-package xterm-color
  :config
  (progn
    (setq comint-output-filter-functions
	(remove 'ansi-color-process-output comint-output-filter-functions))
    (add-hook 'shell-mode-hook
	      ;; ensures it’s always the first one in any buffer, which is
	      ;; important for reasons
              (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))))

(use-package dockerfile-mode)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package wgrep :init (require 'wgrep))

;; Note if it seems like ace-window is acting weird, probably because
;; you also connected via emacsclient
(use-package ace-window
  :config
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (setq aw-ignore-current t)
    ;; use home row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))








(use-package markdown-mode)




;; seems to be creating files
;;(use-package flymake-rust :config (progn
;;				    (add-hook 'rust-mode-hook 'flymake-rust-load)))






(use-package markdown-mode)






;; C/C++

(use-package git-gutter-fringe
  :config
  (progn
    (dolist (p '((git-gutter:added    . "#0c0")
		 (git-gutter:deleted  . "#c00")
		 (git-gutter:modified . "#c0c")))
      (set-face-foreground (car p) (cdr p))
      (set-face-background (car p) (cdr p)))
    (global-git-gutter-mode t)))

(use-package smex
  :config (progn)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package org)

(let ((local-emacs "~/.emacs.local.el"))
  (when (file-readable-p local-emacs)
    (load-file local-emacs)))


;; Setting this in custom.el doesn't work, because that gets loaded
;; _after_ comint gets loaded
(progn
  (setq password-word-equivalents
        '("PIN" "password" "passcode" "passphrase" "pass phrase"))

  (custom-reevaluate-setting 'comint-password-prompt-regexp))






(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)

