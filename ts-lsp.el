(use-package typescript-mode
    :hook (typscript-mode . lsp-deferred))


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
    :config (progn
	      (add-hook 'typescript-mode-hook #'setup-tide-mode)))


(use-package ts-comint)




