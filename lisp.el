;; Set up for Common Lisp development

(use-package slime)
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))
