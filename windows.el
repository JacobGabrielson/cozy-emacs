;;; -*- lexical-binding: t -*-

;;; Extremely basic customizations. These don't need anything outside
;;; of the standard library to be loaded.

;;; Requires
(require 'cl)
(require 'dired-x)
(require 'em-smart)
(require 'uniquify)

;;; Turn off bad modes
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(blink-cursor-mode -1)


;; Turn on good modes
(column-number-mode 1)
(global-font-lock-mode 1)
(global-subword-mode 1)
(global-auto-revert-mode t)
(ido-mode 1)
(show-paren-mode 1)
(display-time)
(windmove-default-keybindings)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
;; ctrl-c left/right
(winner-mode 1)
(toggle-uniquify-buffer-names)

(setq auto-revert-verbose nil)
(setq auto-revert-interval 1)
(setq auto-save-default nil)
(setq compilation-scroll-output t)
(setq create-lockfiles nil)
(setq dabbrev-case-replace nil)
(setq diff-default-read-only t)
(setq dired-auto-revert-buffer t)
(setq dired-dwim-target t)
(setq dired-no-confirm '(create-top-dir))
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval (* 5 1))
(setq echo-keystrokes 0.1)
(setq ediff-keep-variants nil)
(setq enable-recursive-minibuffers t)
(setq eshell-save-history-on-exit t)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-prefer-lisp-functions t)
(setq eww-search-prefix "https://google.com/search?q=")
(setq find-file-existing-other-name t)
(setq find-file-suppress-same-file-warnings t)
(setq font-lock-maximum-decoration t)
(setq global-mark-ring-max 64)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq inhibit-local-variables nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ispell-silently-savep t)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
(setq nxml-slash-auto-complete-flag t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq suggest-key-bindings t)
(setq tab-always-indent 'complete)
(setq vc-follow-symlinks t)
(setq visible-bell t)
(setq windmove-wrap-around t)
(setq woman-use-own-frame nil)

(setq truncate-partial-width-windows nil)
(set-default 'truncate-lines nil)

(setq-default comint-input-ignoredups t)
(setq-default display-line-numbers nil)
(setq-default indicate-empty-lines t)
(setq-default split-width-threshold 160 ; vertical by default
              split-height-threshold nil)

(set-language-environment "UTF-8")
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq comint-buffer-maximum-size 4096)
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

(when (member system-type '(gnu/linux darwin))
  (setq dired-listing-switches "-alhF")) ; .h files before .cpp files

(add-hook 'latex-mode-hook '(lambda () (turn-on-reftex)))

(add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)

;; Die Apple key, die!
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))	; back to meta
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-xc" 'compile)
(global-set-key "\C-xv-" 'ediff-revision)
(global-set-key [remap just-one-space] 'cycle-spacing)
(define-key global-map '[insert] nil)

;; From http://www.emacswiki.org/cgi-bin/wiki/%C3%9Cbersicht/RecentChanges/CopyAndPaste
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(global-set-key [remap suspend-frame] 'ignore)

(lexical-let ((last-shell ""))
  (defun toggle-shell ()
    (interactive)
    (cond ((string-match-p "^\\*shell<[1-9][0-9]*>\\*$" (buffer-name))
           (goto-non-shell-buffer))
          ((get-buffer last-shell) (switch-to-buffer last-shell))
          (t (shell (setq last-shell "*shell<1>*")))))

  (defun switch-shell (n)
    (let ((buffer-name (format "*shell<%d>*" n)))
      (setq last-shell buffer-name)
      (cond ((get-buffer buffer-name)
             (switch-to-buffer buffer-name))
            (t (shell buffer-name)
               (rename-buffer buffer-name)))))

  (defun goto-non-shell-buffer ()
    (let* ((r "^\\*shell<[1-9][0-9]*>\\*$")
           (shell-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
           (non-shells (cl-remove-if shell-buffer-p (buffer-list))))
      (when non-shells
        (switch-to-buffer (first non-shells))))))

(dolist (n (number-sequence 1 9))
  (global-set-key (kbd (concat "M-" (int-to-string n)))
                  (lambda () (interactive) (switch-shell n))))

(defun shellcd ()
  (interactive)
  (let ((shell-dir default-directory))
    (shell) ;; start new one or use existing
    (end-of-buffer) ;; make sure you are at command prompt
    (insert (concat "cd " shell-dir))
    (comint-send-input)))

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m) ; remove ctrl-m from shell output
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; truncate shell buffer to comint-buffer-maximum-size
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; http://stackoverflow.com/a/3072831/68127
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setenv "GIT_PAGER" "cat")
(setenv "PAGER" "cat")

(setq custom-file "~/.emacs-custom.el")
(when (file-readable-p custom-file)
  (load custom-file))


;;; Everything after this point depends on stuff that isn't just built
;;; in to Emacs.

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

(use-package paredit)

(use-package intero
  :config (intero-global-mode 1))

(use-package hindent)

(use-package company
  :config
  (setq company-idle-delay 0
	company-echo-delay 0
	company-dabbrev-downcase nil
	company-minimum-prefix-length 2
	company-selection-wrap-around t
	company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)))

(use-package yaml-mode)

(use-package parinfer)
(use-package markdown-mode)
(use-package elm-mode)

(use-package racer)
(use-package rust-mode
  :config
  (progn
    (setq rust-format-on-save t)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "C-c C-d") #'racer-describe)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)))
(use-package flymake-rust :config (progn
				    (add-hook 'rust-mode-hook 'flymake-rust-load)))
(use-package cargo :config
  (progn
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (add-hook 'cargo-process-mode-hook (lambda ()
					 (visual-line-mode)))))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package markdown-mode)
(use-package elm-mode)
(use-package cider)

;; C/C++
(use-package irony
  :config (progn
	    (add-hook 'c++-mode-hook 'irony-mode)
	    (add-hook 'c-mode-hook 'irony-mode)
	    (add-hook 'objc-mode-hook 'irony-mode)
	    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))
(use-package company-irony
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package flycheck-irony)
(use-package irony-eldoc)

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


(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun smarter-move-end-of-line (arg)
  "If not at end of current line, goes to end of line.
Otherwise, if not at bottom of current window, goes to bottom of
window.  Otherwise, goes to end of buffer."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (backward-line (1- arg))))
  
  (let ((old-point (point)))
    (if (not (eolp))
	(end-of-line)
      (move-to-window-line -1)
      (move-end-of-line 1)
      (if (eq old-point (point))
          (goto-char (point-max))))))

(global-set-key [remap move-end-of-line] 'smarter-move-end-of-line)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(let ((local-emacs "~/.emacs.local.el"))
  (when (file-readable-p local-emacs)
    (load-file local-emacs)))


;; Setting this in custom.el doesn't work, because that gets loaded
;; _after_ comint gets loaded
(progn
  (setq password-word-equivalents
        '("PIN" "password" "passcode" "passphrase" "pass phrase"))

  (custom-reevaluate-setting 'comint-password-prompt-regexp))

(dolist (hook '(
		rust-mode-hook
		lisp-interaction-mode-hook
		haskell-mode-hook
		go-mode-hook
		))
  (add-hook hook (lambda ()
		   (set-variable 'display-line-numbers t t))))


(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)
