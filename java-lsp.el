(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook #'linum-mode)
;; need to set this
;; (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home/bin/java")

