;; from http://www.kotaweaver.com/blog/emacs-python-lsp/

;;(setq lsp-pyls-plugins-flake8-exclude '("E501"))
(setq lsp-pylsp-plugins-flake8-exclude '("E501"))

;;(setq lsp-pyls-plugins-flake8-exclude nil)
(setq lsp-pylsp-plugins-flake8-exclude nil)

;;(setq lsp-pyls-plugins-pycodestyle-ignore '("D100"))
;;(setq lsp-pyls-plugins-pycodestyle-ignore '("D100" "E501"))
(setq lsp-pylsp-plugins-pycodestyle-ignore '("D100" "E501"))

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


(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
(setq dap-python-debugger 'debugpy)
(setq dap-python-executable "python3")

(add-hook 'python-mode-hook 'indent-guide-mode)

(defun pm-display-line-numbers ()
  (setq display-line-numbers t))

(add-hook 'python-mode-hook 'pm-display-line-numbers)
