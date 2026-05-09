;;; -*- lexical-binding: t -*-

;;; Go development via gopls / lsp-mode.
;;; References:
;;;   https://arenzana.org/2019/12/emacs-go-mode-revisited/
;;;   https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;;;   https://lupan.pl/dotemacs/

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
