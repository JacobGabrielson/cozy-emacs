;; from http://www.kotaweaver.com/blog/emacs-python-lsp/

;;(setq lsp-pyls-plugins-flake8-exclude '("E501"))
(setq lsp-pylsp-plugins-flake8-exclude '("E501"))

;;(setq lsp-pyls-plugins-flake8-exclude nil)
;;(setq lsp-pylsp-plugins-flake8-exclude nil)

;;(setq lsp-pyls-plugins-pycodestyle-ignore '("D100"))
;;(setq lsp-pyls-plugins-pycodestyle-ignore '("D100" "E501"))
(setq lsp-pylsp-plugins-pycodestyle-ignore '("D100" "D107" "E501"))

(setq lsp-pylsp-plugins-pydocstyle-add-ignore '("D100" "D101" "D107" "E501"))

(setq lsp-pyls-plugins-pycodestyle-enabled nil)

;;(setq lsp-pylsp-plugins-pydocstyle-add-ignore nil)



;;(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
;;(setq dap-python-debugger 'debugpy)
;;(setq dap-python-executable "python3")

(add-hook 'python-mode-hook 'indent-guide-mode)

(defun pm-display-line-numbers ()
  (direnv-update-directory-environment)
  (setq display-line-numbers t))

(add-hook 'python-mode-hook 'pm-display-line-numbers)
;; See https://github.com/emacs-lsp/lsp-mode/pull/3975 for why this
;; might not always work?
(add-hook 'python-mode-hook 'lsp-deferred)
