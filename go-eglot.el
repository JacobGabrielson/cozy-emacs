


(use-package eglot)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (progn
    (setq compilation-read-command t)
    (add-hook 'go-mode-hook 'eglot-ensure)))

