

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq
   lsp-auto-guess-root t
   lsp-eldoc-render-all t
   lsp-enable-file-watchers nil ; too annoying/perf issues
   ;;lsp-keymap-prefix "<s-z>"	;; no longer working
   lsp-lens-enable t
   lsp-modeline-diagnostics-scope :project
   lsp-signature-auto-activate nil
   lsp-ui-sideline-delay 0.5
   lsp-ui-sideline-enable t
   lsp-ui-doc-enable t
   lsp-ui-imenu-enable t
   lsp-ui-peek-enable t
   lsp-idle-delay 0.500
   ))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (progn
    (setq lsp-ui-sideline-show-diagnostics t
	  lsp-ui-sideline-show-hover t
	  lsp-ui-sideline-show-code-actions t
	  )))

(use-package lsp-ivy :ensure t)
(use-package helm-lsp :ensure t
  :config
  (setq helm-use-frame-when-more-than-two-windows nil))
(use-package lsp-treemacs :ensure t)
