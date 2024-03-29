(use-package paredit)

(use-package intero
  :config (intero-global-mode 1))

(use-package hindent)

(use-package parinfer)
(use-package racer)
(use-package rust-mode
  :config
  (progn
    (setq rust-format-on-save t)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "C-c C-d") #'racer-describe)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)))
(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package elm-mode)
(use-package cargo :config
  (progn
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (add-hook 'cargo-process-mode-hook (lambda ()
					 (visual-line-mode)))))

(use-package cider)

(use-package irony
  :config (progn
	    (add-hook 'c++-mode-hook 'irony-mode)
	    (add-hook 'c-mode-hook 'irony-mode)
	    (add-hook 'objc-mode-hook 'irony-mode)
	    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))
(use-package company-irony
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package flycheck-irony)
(use-package irony-eldoc)


(dolist (hook '(
		rust-mode-hook
		lisp-interaction-mode-hook
		haskell-mode-hook
		go-mode-hook
		shell-mode-hook
		))
  (add-hook hook (lambda ()
		   (set-variable 'display-line-numbers t t))))
