;; Set up for Common Lisp development

(use-package slime)
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(add-to-list 'slime-contribs 'slime-repl)
(add-to-list 'slime-contribs 'slime-autodoc)
(add-to-list 'slime-contribs 'slime-c-p-c)
(add-to-list 'slime-contribs 'slime-editing-commands)
(add-to-list 'slime-contribs 'slime-references)
(add-to-list 'slime-contribs 'slime-scratch)
(add-to-list 'slime-contribs 'slime-asdf)
(add-to-list 'slime-contribs 'slime-fuzzy)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))
