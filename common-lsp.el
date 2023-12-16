

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (progn
    (setq
     lsp-auto-guess-root t
     lsp-eldoc-render-all t
     lsp-enable-file-watchers nil ; too annoying/perf issues
     lsp-keymap-prefix "s-z"	;; otherwise lock screen
     lsp-lens-enable t
     lsp-modeline-diagnostics-scope :project
     lsp-signature-auto-activate nil
     lsp-ui-sideline-delay 0.5
     lsp-ui-sideline-enable t
     lsp-ui-doc-enable t
     lsp-ui-imenu-enable t
     lsp-ui-peek-enable t
     )))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (progn
    (setq lsp-ui-sideline-show-diagnostics t
	  lsp-ui-sideline-show-hover t
	  lsp-ui-sideline-show-code-actions t
	  )))

(use-package lsp-ivy :ensure t)
(use-package helm-lsp :ensure t)
;; lsp-ivy-diagnostics

(use-package lsp-treemacs :ensure t)



