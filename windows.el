;;; Extremely basic customizations. These don't need anything outside
;;; of the standard library to be loaded.

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
(load-theme 'manoj-dark)
(display-time)
(require 'windmove)
(windmove-default-keybindings)
(require 'cl)
(require 'dired-x)
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
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
(setq bookmark-save-flag 1)
(setq compilation-scroll-output t)
(setq create-lockfiles nil)
(setq dabbrev-case-replace nil)
(setq diff-default-read-only t)
(setq dired-auto-revert-buffer t)
(setq dired-dwim-target t)
(setq dired-no-confirm '(create-top-dir))
(setq display-time-24hr-format t)
(setq display-time-interval (* 5 1))
(setq ediff-keep-variants nil)
(setq enable-recursive-minibuffers t)
(setq find-file-existing-other-name t)
(setq find-file-suppress-same-file-warnings t)
(setq font-lock-maximum-decoration t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq inhibit-local-variables nil)
(setq inhibit-startup-message t)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq require-final-newline t)
(setq suggest-key-bindings t)
(setq tab-always-indent 'complete)
(setq visible-bell t)
(setq windmove-wrap-around t)
(setq woman-use-own-frame nil)
(setq-default comint-input-ignoredups t)
(setq-default display-line-numbers t)
(setq-default indicate-empty-lines t)

(add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)

(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-xc" 'compile)
(global-set-key "\C-xv-" 'ediff-revision)
(define-key global-map (kbd "<C-tab>") 'other-frame)

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
(setq
 uniquify-buffer-name-style 'forward
 uniquify-separator "/")
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m) ; remove ctrl-m from shell output
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; truncate shell buffer to comint-buffer-maximum-size

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


(use-package magit
  :ensure t
  :bind   (
           ("C-c p" . 'magit-find-file-completing-read)
           ("C-x g" . magit-status)))

(use-package wgrep :ensure t :init (require 'wgrep))

(use-package paredit :ensure t)
(use-package intero
  :ensure t
  :config (progn
	    (intero-global-mode 1)
	    ))
(use-package hindent :ensure t)
(use-package company :ensure t)
(use-package yaml-mode :ensure t)
(use-package rust-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package smex :ensure t
  :config (progn
	    (smex-initialize)
	    (global-set-key (kbd "M-x") 'smex)
	    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
	    ;; This is your old M-x.
	    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))


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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(setq custom-file "~/.emacs-custom.el")
(when (file-readable-p custom-file)
  (load custom-file))
