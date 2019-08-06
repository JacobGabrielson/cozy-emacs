;;; -*- lexical-binding: t -*-

;;; Extremely basic customizations. These don't need anything outside
;;; of the standard library to be loaded.

(setq custom-file "~/.emacs-custom.el")

(require 'cl)
(require 'dired-x)
(require 'em-smart)
(require 'uniquify)

;;; Turn off bad modes
(unless window-system
  (menu-bar-mode -1))
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
(global-hl-line-mode 1)
(size-indication-mode t)
(save-place-mode 1)
(savehist-mode 1)
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

(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-autosave-interval 60)
(setq hl-line-sticky-flag nil)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 1)
(setq auto-save-default nil)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )
(setq create-lockfiles nil)
(setq dabbrev-case-replace nil)
(setq diff-default-read-only t)
(setq dired-auto-revert-buffer t)
(setq dired-dwim-target t)
(setq dired-no-confirm '(create-top-dir))
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval (* 5 1))
(setq echo-keystrokes 0.02)

(setq
 scroll-preserve-screen-position t
 scroll-margin 0
 scroll-conservatively 101)

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
(setq history-length 1000)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq inhibit-local-variables nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ispell-silently-savep t)
(setq kill-whole-line t)
(setq kill-do-not-save-duplicates t)

(setq confirm-kill-processes nil)

(setq  global-auto-revert-non-file-buffers t)


(setq  save-interprogram-paste-before-kill t)

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
(setq ffap-machine-p-known 'reject)
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
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

 


(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq comint-buffer-maximum-size 4096)
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)

(fset 'yes-or-no-p 'y-or-n-p)
(defalias #'view-hello-file #'ignore)

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

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)

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
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

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

(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key [remap move-end-of-line] 'smarter-move-end-of-line)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; Theme and font settings
(when window-system
  (defun text-scale-default () (interactive) (text-scale-set 0))
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "s-0") 'text-scale-default))


(when (file-readable-p custom-file)
  (load custom-file))
