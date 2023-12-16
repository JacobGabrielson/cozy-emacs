;; From
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; https://lupan.pl/dotemacs/


(defun custom-dap-stopped-hook (ignore)
  (call-interactively #'dap-hydra))

;; (use-package dap-mode
;;   :ensure nil
;;   :config
;;   (progn
;;     (require 'dap-go)
;;     ;; note, once ... manually call:
;;     ;(dap-go-setup)
;;     (add-hook 'dap-stopped-hook 'custom-dap-stopped-hook)))


;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;(add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(defun custom-go-mode ()
  (lsp-go-install-save-hooks)
  (lsp-headerline-breadcrumb-mode)
  (lsp-modeline-code-actions-mode)
  (lsp-treemacs-sync-mode)
  (local-set-key (kbd "M-?") 'lsp-find-references))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (progn
    (setq compilation-read-command t)
    (add-hook 'go-mode-hook 'custom-go-mode)))

(use-package go-playground
  :ensure t)

(add-hook 'go-mode-hook 'lsp-deferred)
