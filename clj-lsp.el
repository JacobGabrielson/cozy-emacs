;;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

(use-package cider)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)


