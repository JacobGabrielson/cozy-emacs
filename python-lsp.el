;; from http://www.kotaweaver.com/blog/emacs-python-lsp/

;;(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
;;(setq dap-python-debugger 'debugpy)
;;(setq dap-python-executable "python3")

(add-hook 'python-mode-hook 'indent-guide-mode)

;; (use-package auto-virtualenv
;;   :ensure t
;;   :init
;;   (use-package pyvenv
;;     :ensure t)
;;   :config
;;   (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;;   (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

(defun pm-display-line-numbers ()
  (setq display-line-numbers t))

(add-hook 'python-mode-hook 'pm-display-line-numbers)
;; See https://github.com/emacs-lsp/lsp-mode/pull/3975 for why this
;; might not always work?
(add-hook 'python-mode-hook 'lsp-deferred)

;; Create Python processes per-project, not global
(setq python-shell-dedicated 'project)
