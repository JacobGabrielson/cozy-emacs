
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package eglot)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (progn
    (setq compilation-read-command t)
    (add-hook 'go-mode-hook 'eglot-ensure)))

