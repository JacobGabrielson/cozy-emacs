;;; -*- lexical-binding: t -*-

;;; Common Lisp development via SLIME.

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :config
  ;; Quicklisp's SLIME helper, if present. Sets up its own load-path
  ;; entries for slime-autoloads.
  (let ((helper (expand-file-name "~/quicklisp/slime-helper.el")))
    (when (file-exists-p helper)
      (load helper)))
  ;; `slime-setup' both registers the contribs and runs each one's
  ;; *-init function, which `add-to-list 'slime-contribs ...' would skip.
  (slime-setup '(slime-repl
                 slime-autodoc
                 slime-c-p-c
                 slime-editing-commands
                 slime-references
                 slime-scratch
                 slime-asdf
                 slime-fuzzy)))

;; Enable Common Lisp blocks in org-babel additively, so we don't clobber
;; the languages packages.el already registered (shell, plantuml, gnuplot).
(with-eval-after-load 'org
  (require 'ob-lisp)
  (add-to-list 'org-babel-load-languages '(lisp . t)))
