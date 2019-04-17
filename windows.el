;;; Extremely basic customizations. These don't need anything outside
;;; of the standard library to be loaded.
(require 'cl)
(require 'dired-x)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(column-number-mode 1)
(global-font-lock-mode 1)
(global-subword-mode 1)
(blink-cursor-mode -1)
(global-auto-revert-mode t)
(ido-mode 1)
(show-paren-mode 1)
(display-time)
(windmove-default-keybindings)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
;; Die Apple key, die!
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))	; back to meta
;; ctrl-c left/right
(winner-mode 1)
(toggle-uniquify-buffer-names)

(setq auto-revert-verbose nil)
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
(setq ediff-keep-variants nil)
(setq enable-recursive-minibuffers t)
(setq eshell-save-history-on-exit t)
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

(setq-default comint-input-ignoredups t)
(setq-default display-line-numbers nil)
(setq-default indicate-empty-lines t)

(fset 'yes-or-no-p 'y-or-n-p)

(when (member system-type '(gnu/linux darwin))
  (setq dired-listing-switches "-alhF")) ; .h files before .cpp files

(add-hook 'latex-mode-hook '(lambda () (turn-on-reftex)))


(add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)

(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-xc" 'compile)
(global-set-key "\C-xv-" 'ediff-revision)
(define-key global-map (kbd "<C-tab>") 'other-frame)
(define-key global-map '[insert] nil)

;; From http://www.emacswiki.org/cgi-bin/wiki/%C3%9Cbersicht/RecentChanges/CopyAndPaste
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(defun shellcd ()
  (interactive)
  (let ((shell-dir default-directory))
    (shell) ;; start new one or use existing
    (end-of-buffer) ;; make sure you are at command prompt
    (insert (concat "cd " shell-dir))
    (comint-send-input)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m) ; remove ctrl-m from shell output
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; truncate shell buffer to comint-buffer-maximum-size
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

;;; Begin from https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;;; End from https://melpa.org/#/getting-started

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package magit
    :bind ("C-x g" . magit-status))

(use-package wgrep :init (require 'wgrep))

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
(use-package company)
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
(use-package cargo :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package markdown-mode)
(use-package elm-mode)
(use-package cider)
(use-package irony)

(use-package smex
    :config (progn)
        (smex-initialize)
        (global-set-key (kbd "M-x") 'smex)
        (global-set-key (kbd "M-X") 'smex-major-mode-commands)
        ;; This is your old M-x.
        (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


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

(when window-system
  ;; Prevent annoying minimizing
  (global-unset-key "\C-z"))

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(let ((local-emacs "~/.emacs.local.el"))
  (when (file-readable-p local-emacs)
    (load-file local-emacs)))
