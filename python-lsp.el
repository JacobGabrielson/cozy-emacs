;; from http://www.kotaweaver.com/blog/emacs-python-lsp/

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

; setup Emacs path from our ~/.bashrc
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

					; we also should make sure we have flycheck installed
(use-package flycheck
  :ensure t)

; Let's set up company! perhaps not necessary but this is what i like to use
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

; install lsp mode
(use-package lsp-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

; let's add the lsp company backend
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

; also installs lsp as a dependency
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))
