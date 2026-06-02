;;; -*- lexical-binding: t -*-

;;; Go development via gopls / lsp-mode.
;;; References:
;;;   https://arenzana.org/2019/12/emacs-go-mode-revisited/
;;;   https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;;;   https://lupan.pl/dotemacs/

;; This module builds on the shared lsp-mode setup in common-lsp.el, which
;; installs lsp-mode / lsp-treemacs and autoloads `lsp-deferred'. Make sure
;; that's loaded first regardless of the order modules are loaded from
;; ~/.emacs, otherwise opening a .go file errors with
;; `(void-function lsp-deferred)'.
(unless (fboundp 'lsp-deferred)
  (load "common-lsp"))

;; `go install ...@latest' drops binaries (gopls, etc.) into $GOPATH/bin,
;; which GUI Emacs on macOS and login shells don't put on PATH. Without it
;; `executable-find' can't see gopls even right after installing it, so
;; lsp-mode keeps re-offering to install it. Add $GOPATH/bin to both
;; `exec-path' (used by `executable-find') and the PATH env var inherited by
;; the gopls subprocess.
(let ((gobin (expand-file-name "bin" (or (getenv "GOPATH")
                                         (expand-file-name "~/go")))))
  (when (file-directory-p gobin)
    (add-to-list 'exec-path gobin)
    (let ((path (getenv "PATH")))
      (unless (and path (string-match-p (regexp-quote gobin) path))
        (setenv "PATH" (concat gobin path-separator path))))))

(defun lsp-go-install-save-hooks ()
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;(add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

(defun custom-go-mode ()
  (lsp-go-install-save-hooks)
  (lsp-headerline-breadcrumb-mode)
  (lsp-modeline-code-actions-mode)
  (lsp-treemacs-sync-mode))

(use-package go-mode
  :after yasnippet
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ("M-?" . lsp-find-references))
  :init
  (setq compilation-read-command t)
  (add-hook 'go-mode-hook 'custom-go-mode)
  (add-hook 'go-mode-hook 'lsp-deferred)
  :hook (go-mode . yas-minor-mode))

(use-package go-playground)
