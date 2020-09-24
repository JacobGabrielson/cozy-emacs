;; From
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; https://lupan.pl/dotemacs/

;; otherwise it's Windows-L which is Lock Screen
(setq lsp-keymap-prefix "s-z")

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :init
  (progn
     (setq lsp-modeline-diagnostics-scope :project)
  ))

(use-package lsp-ivy)


;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;(add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

;;Optional - provides fancier overlays.

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (progn
    (setq lsp-ui-sideline-show-diagnostics t
	  lsp-ui-sideline-show-hover t
	  lsp-ui-sideline-show-code-actions t
	  )))

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))


;; no longer supported, per 
;;(use-package company-lsp
;;  :ensure t
;;  :commands company-lsp)


;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package lsp-treemacs)

(defun custom-go-mode ()
  (lsp-go-install-save-hooks)
  (lsp-headerline-breadcrumb-mode)
  (lsp-modeline-code-actions-mode)
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
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

(setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t
      lsp-gopls-staticcheck t
      lsp-eldoc-render-all t
      lsp-ui-sideline-delay 0.5
      lsp-gopls-complete-unimported t)
