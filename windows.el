(require 'cl)

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


(use-package magit :ensure t)
(use-package intero :ensure t)

(setq require-final-newline t)
(setq visible-bell t)
(setq require-final-newline t)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)



(ido-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'whiteboard)

(setq-default display-line-numbers t)
(display-time)



(setq custom-file "~/.emacs-custom.el")
(load custom-file)
