(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)
;;(remove-hook 'java-mode-hook #'lsp)

(add-hook 'java-mode-hook #'linum-mode)
;; need to set this
;; (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home/bin/java")


;; add'l config to look at:

;; https://github.com/MatthewZMD/.emacs.d#orgf8b7fe2
;; https://pastebin.com/Pb9K8StS
;; https://github.com/neppramod/emacs-configuration/blob/master/configuration.org
;; https://github.com/neppramod/java_emacs/blob/master/emacs-configuration.org

