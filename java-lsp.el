
(use-package lsp-java :ensure t :after lsp yasnippet)
(add-hook 'java-mode-hook #'lsp)
;;(remove-hook 'java-mode-hook #'lsp)

(defun my-java-mode-hook ()
  (flycheck-mode)
  (subword-mode)
  (yas-minor-mode)
  (when window-system
    (set-fringe-style '(8 . 0)))

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)


;; need to set this in ~/.emacs
;; (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home/bin/java")

;; lombok won't work without this (or equiv)
;; https://github.com/kevinziegler/lsp-java-lombok/blob/master/lsp-java-lombok.el

;; add'l config to look at:

;; see https://gitlab.com/skybert/my-little-friends/-/tree/master/emacs
;; https://github.com/MatthewZMD/.emacs.d#orgf8b7fe2
;; https://pastebin.com/Pb9K8StS
;; https://github.com/neppramod/emacs-configuration/blob/master/configuration.org
;; https://github.com/neppramod/java_emacs/blob/master/emacs-configuration.org

