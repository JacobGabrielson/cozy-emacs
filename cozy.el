;; To use this file, add this to your ~/.emacs file:
;;
;;   (add-to-list 'load-path "/Path/to/trunk/Cozi/Tools/Emacs") ; or wherever it's checked out to
;;   (load "cozi")
;;
;; What it does:
;;
;;   * Sets some basic Cozi standard formatting configuration (e.g., 4
;;     spaces, no tabs).
;;
;;   * Enhances JavaScript editing mode significantly (using js2.el).
;;
;;   * Enhances Python editing mode significantly.
;;
;;   * Enhances Erlang editing mode significantly.
;;
;;   * Enhances Subversion support (using psvn.el).
;;
;;   * Enhances XML editing (using nxml-mode).
;;
;;   * Sets a bunch of no-brainer customizations that you definitely
;;     want.
;;
;;   * Makes w3m (the web browser) work better.
;;
;; Dependencies:
;;
;;   * Works much better (for Erlang) if you've installed distel:
;;     http://code.google.com/p/distel/
;;
;;   * See http://www.socialtext.net/cozi/index.cgi?erlang for help on
;;     setting up distel.

(require 'cl)

(defmacro* when-let ((var value) &body body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))

;; BUFFER-FILE-NAME in case we're doing EVAL- type stuff instead of
;; LOAD.
(defvar cozi/*load-directory* (file-name-directory (or load-file-name
                                                       buffer-file-name)))

(defvar cozi/*third-party-directory* (file-name-directory (concat cozi/*load-directory* "third-party/"))
  "Some useful utilities that aren't part of the standard Emacs
  distribution (yet).")

(defvar cozi/*nxml-mode-directory* (file-name-directory (concat cozi/*third-party-directory* "nxml-mode/"))
  "A better XML mode.")

(when (file-directory-p cozi/*third-party-directory*)
  (if (>= emacs-major-version 23)
      ;; Emacs 23 appears to have newer python mode so don't override
      (add-to-list 'load-path cozi/*third-party-directory* t)
    ;; Emacs 22 python mode kind of sucks, so override
    (pushnew cozi/*third-party-directory* load-path))

  ;; nxml should work everywhere
  (pushnew cozi/*nxml-mode-directory* load-path)

  (when (file-writable-p cozi/*third-party-directory*)
    ;; Won't do anything if the .elc's are up-to-date.  js2.el *must*
    ;; be byte-compiled so let's try to make things 'just work' for
    ;; everyone.
    ;;
    ;; Note: this handles all subdirectories automatically.
    (byte-recompile-directory cozi/*third-party-directory* 0)
    (delete-windows-on "*Compile-Log*"))

  (load "rng-auto")                     ; for nxml-mode
  (setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|csproj\\|wdproj\\)\\'$" . nxml-mode)
	      auto-mode-alist))

  ;; Make sure our patched emacs.py gets picked up.
  (let ((path (getenv "PYTHONPATH")))
    (setenv "PYTHONPATH"
            (if path
                (concat cozi/*third-party-directory* path-separator path)
              cozi/*third-party-directory*))))


(defun cozi/script-path (script)
  (let ((full-path (concat cozi/*load-directory* script)))
    (unless (file-executable-p full-path)
      (error "cozi.el: %s does not exist, or is not executable" full-path))
    full-path))

(defun cozi/first-executable (&rest file-list)
  (find-if #'file-executable-p (mapcar #'expand-file-name file-list)))

(defun cozi/first-directory (&rest file-list)
  (find-if #'file-directory-p (mapcar #'expand-file-name file-list)))

(defun cozi/directories-that-exist (&rest dirs)
  (remove-if-not #'file-directory-p dirs))

(mapc #'(lambda (path)
          (pushnew path load-path))
      (nreverse                  ; to get them in same order as listed
       (cozi/directories-that-exist
        "/opt/erlang-R12B-3/lib/erlang/lib/tools-2.6.1/emacs"
        "/usr/local/lib/erlang/lib/tools-2.6.1/emacs"
        "/usr/lib/erlang/lib/tools-2.6.1/emacs"
        "c:/progra~1/erl5.6.1/lib/tools-2.6.1/emacs")))


;; This is ok to load because all it does is autoload other stuff.
(load "erlang-start" 'no-error)

(pushnew '("\\.app\\'" . erlang-mode) auto-mode-alist)

(eval-after-load "erlang"
  '(progn
     (require 'flymake)
     (setq erlang-root-dir (cozi/first-directory
                            "/opt/erlang-R12B-3"
                            "/usr/local/lib/erlang"
                            "/usr/lib/erlang"
                            "c:/progra~1/erl5.6.1"))

     (when-let (erlang-bin (cozi/first-directory
                            "/opt/erlang-R12B-3/bin"
                            "/usr/local/lib/erlang/bin"
                            "/usr/lib/erlang/bin"
                            "c:/progra~1/erl5.6.1/bin"))
               (add-to-list 'exec-path erlang-bin))
     (load "distel" :noerror)
     (add-hook 'erlang-mode-hook
               (lambda ()
                 (define-key erlang-mode-map (kbd "M-p") 'flymake-goto-prev-error)
                 (define-key erlang-mode-map (kbd "M-n") 'flymake-goto-next-error)
                 (when (buffer-file-name)
                   ;; Flymake messes up the Distel debugger in a
                   ;; hard-to-diagnose way.  Fortunately it's easy to
                   ;; recognize such buffers as they have no
                   ;; associated file name.
                   (flymake-mode 1))
                 t))))

(eval-after-load "distel"
  '(progn
     (distel-setup)
     (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
     (add-hook 'erlang-mode-hook
               (lambda ()
                 ;; When starting an Erlang shell in Emacs, default in
                 ;; the node name.  Pass -name, not -sname so you
                 ;; could talk to this node from other machines.
                 (setq inferior-erlang-machine-options '("-name" "emacs"))
                 ;; Add Erlang functions to an imenu menu
                 (imenu-add-to-menubar "imenu")))

     (defvar *erlang-shell-distel-keys* '(erl-complete
                                          erl-find-source-under-point
                                          erl-find-source-unwind
                                          erl-process-list
                                          erl-ie-show-session
                                          erl-fdoc-describe
                                          erl-fdoc-apropos
                                          erl-who-calls
                                          erl-openparen)
       "Distel functions useful in the Erlang shell.")

     (add-hook 'erlang-shell-mode-hook
               ;; Add Distel bindings to the Erlang shell.
               (lambda ()
                 ;; Comint binding that conflicts with some of the
                 ;; distel keybindings.
                 (define-key erlang-shell-mode-map (kbd "C-c C-d") nil)

                 (loop for (key-binding function) in distel-keys
                       when (memq function *erlang-shell-distel-keys*)
                       do (progn
                            (message "Adding keybinding %s for %s"
                                     (format-kbd-macro key-binding) function)
                            (define-key erlang-shell-mode-map key-binding function)))))))

(eval-after-load "flymake"
  '(progn
     ;; Examples of how you can change this yourself
     ;;(set-face-attribute 'flymake-errline nil :underline t :background "gray14")
     ;;(set-face-attribute 'flymake-warnline nil :underline t :background "black")

     (defun credmp/flymake-display-err-minibuf ()
       "Displays the error/warning for the current line in the echo area"
       (interactive)
       (let* ((line-no             (flymake-current-line-no))
              (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
              (count               (length line-err-info-list)))
         (while (plusp count)
           (when line-err-info-list
             (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
                    (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                    (text       (flymake-ler-text (nth (1- count) line-err-info-list)))
                    (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
               (message "[%s] %s" line text)))
           (setq count (1- count)))))

     (defadvice flymake-goto-next-error (after show-error-minibuffer activate)
       (credmp/flymake-display-err-minibuf))

     (defadvice flymake-goto-prev-error (after show-error-minibuffer activate)
       (credmp/flymake-display-err-minibuf))

     (defun flymake-erlang-init ()
       ;;(message "running flymake-erlang-init")
       (let ((local-directory (file-name-directory buffer-file-name)))
         ;;(message "%s" local-directory)
         (if (file-writable-p local-directory)
             (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                'flymake-create-temp-inplace))
                    (local-file (file-relative-name
                                 temp-file
                                 local-directory)))
               (list (cozi/script-path "flyalyzer") (list local-file)))
           (list "cat" (list "/dev/null")))))

     (defun flymake-pylint-init ()
       ;;(message "running flymake-pylint-init")
       (let ((local-directory (file-name-directory buffer-file-name)))
         (if (file-writable-p local-directory)
             (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                'flymake-create-temp-inplace))
                    (local-file (file-relative-name
                                 temp-file
                                 local-directory)))
               (list (cozi/script-path "epylint") (list local-file)))
           (list "cat" (list "/dev/null")))))

     (when (executable-find "gmcs")
       (defun flymake-gmcs-init ()
         ;;(message "running flymake-gmcs-init")
         (let ((local-directory (file-name-directory buffer-file-name)))
           (if (file-writable-p local-directory)
               (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                  'flymake-create-temp-inplace))
                      (local-file (file-relative-name
                                   temp-file
                                   local-directory)))
                 (list "gmcs" (list "--parse" local-file)))
             (list "cat" (list "/dev/null")))))
       (add-to-list 'flymake-allowed-file-name-masks '("\\.cs\\'" flymake-gmcs-init)))

     ;; neither "epylint" nor "cat" works on windows-nt
     (unless (eq system-type 'windows-nt)
       (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init)))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

     ))

(eval-after-load "python"
  '(progn
     (require 'flymake)
     ;; Would like to use the following but they cause
     ;;  (error Lisp nesting exceeds `max-lisp-eval-depth')
     ;;(require 'pymacs)
     ;;(pymacs-load "ropemacs" "rope-")

     ;; SLIME-like key bindings
     (define-key python-mode-map (kbd "M-.") 'python-find-function)
     (define-key python-mode-map (kbd "M-p") 'flymake-goto-prev-error)
     (define-key python-mode-map (kbd "M-n") 'flymake-goto-next-error)
     (define-key python-mode-map (kbd "C-M-x") 'python-send-defun)
     (define-key python-mode-map (kbd "C-c C-k") 'python-load-file)
     (when (< emacs-major-version 23)
       (define-key python-mode-map (kbd "C-c C-d h") 'python-describe-symbol)
       (define-key python-mode-map (kbd "C-c C-d C-h") 'python-describe-symbol))
     (setq python-python-command (or (cozi/first-executable
                                      "/usr/local/bin/python"
                                      "g:/Python25/python.exe")
                                     python-python-command))
     (add-hook 'python-mode-hook
               '(lambda ()
                  (eldoc-mode 1)
                  (flymake-mode 1)
                  t))

     (add-hook 'pdb-mode-hook
               (lambda ()
                 (gud-tooltip-mode 1)))

     ;; .egg files are just .zip files, after all
     (add-to-list 'auto-mode-alist (cons (rx ".egg" eos) 'archive-mode))))

;;; JavaScript Support

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; C# Support

(eval-after-load "csharp-mode"
  '(progn
     (require 'flymake)
     (add-hook 'csharp-mode-hook
               '(lambda ()
                  (flymake-mode 1)
                  ;; DEFINE-KEY style above isn't working on
                  ;; csharp-mode for some reason, so do it this way.
                  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
                  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
                  t))))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Similar to TortoiseSVN, in an Emacs kind of way.  Seems to play
;; nice with Emacs' built-in Subversion support.
(require 'psvn)


;;; SQL support

(eval-after-load "sql"
  '(progn
     ;; We use MySQL at Cozi so save you the step of doing it
     (sql-set-product 'mysql)
     (setq sql-mysql-options '("-C" "-t" "-f" "-n"))
     ))


;;; w3m stuff

(when (executable-find "w3m")
  (autoload 'w3m "w3m" nil t)
  (autoload 'w3m-browse-url "w3m" nil t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-use-form t
        w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8
        w3m-fill-column 100
        w3m-default-display-inline-images t
        w3m-favicon-use-cache-file t
        w3m-use-cookies t)

  ;; Fixes some weirdness when displaying web pages.  From emacswiki
  ;; w3m stuff.
  (standard-display-ascii ?\200 [15])
  (standard-display-ascii ?\201 [21])
  (standard-display-ascii ?\202 [24])
  (standard-display-ascii ?\203 [13])
  (standard-display-ascii ?\204 [22])
  (standard-display-ascii ?\205 [25])
  (standard-display-ascii ?\206 [12])
  (standard-display-ascii ?\210 [23])
  (standard-display-ascii ?\211 [14])
  (standard-display-ascii ?\212 [18])
  (standard-display-ascii ?\214 [11])
  (standard-display-ascii ?\222 [?\'])
  (standard-display-ascii ?\223 [?\"])
  (standard-display-ascii ?\224 [?\"])
  (standard-display-ascii ?\225 [?+])
  (standard-display-ascii ?\227 " -- ")
  )



;;; Misc settings

(setq flymake-log-level 0) ; ERRORS should go to *Messages* buffer
(line-number-mode 1)
(show-paren-mode 1)
(iswitchb-mode 1)
(icomplete-mode 1)
(transient-mark-mode 1)
(setq bookmark-save-flag 1)
(setq font-lock-maximum-decoration t)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq suggest-key-bindings t)
(setq diff-default-read-only t) ; then you can use 'n', 'p' to move around
(setq nxml-slash-auto-complete-flag t)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4) ; js2-mode (and C-like other) modes will follow this
(setq nxml-slash-auto-complete-flag t)
(setq diff-default-read-only t)
(global-auto-revert-mode t)
(setq bookmark-save-flag 1)
(setq-default indicate-empty-lines t)
(setq truncate-partial-width-windows nil)
(setq-default comint-input-ignoredups t)
;;TODO: customize clean-buffer-list-* stuff before re-enabling
;(require 'midnight)

;;; Key bindings

(defun smart-beginning-of-line (count)
  "Go to beginning of line, unless already there.

If already at beginning, go to first non-whitespace character on
line."
  (interactive "p")
  (when (or (null count)
            (= count 0))
    (setq count 1))
  (if (and (bolp)
           (= count 1))
      (skip-chars-forward " \t")
    (beginning-of-line count)))

(defun smart-end-of-line (count)
  (interactive "p")
  (when (or (null count)
            (= count 0))
    (setq count 1))
  (if (and (eolp)
           (= count 1))
      (skip-chars-backward " \t")
    (end-of-line count)))

;; Make it so that if you hit C-a twice it skips leading whitespace
(global-set-key "\C-a" 'smart-beginning-of-line)
(global-set-key "\C-e" 'smart-end-of-line)

;; So that SQL buffers have an actually useful name rather than just
;; *SQL*.
(add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)

(defun python-add-to-all ()
  "Take the symbol under the point and add it to the __all__
list, if it's not already there."
  (interactive)
  (save-excursion
    (let ((thing (thing-at-point 'symbol)))
      (when thing
        (if (progn (goto-char (point-min))
                   (let (found)
                     (while (and (not found)
                                 (re-search-forward (rx symbol-start "__all__" symbol-end 
                                                        (0+ space) "=" (0+ space) 
                                                        (syntax open-parenthesis))
                                                    nil t))
                       (setq found (not (python-in-string/comment))))
                     found))
            (when (not (looking-at (rx-to-string
                                    `(and (0+ (not (syntax close-parenthesis)))
                                          (syntax string-quote) ,thing (syntax string-quote)))))
              (insert (format "\'%s\', " thing)))
          (beginning-of-buffer)
          ;; Put before any import lines, or if none, the first class or
          ;; function.
          (when (not (re-search-forward (rx bol (or "import" "from") symbol-end) nil t))
            (re-search-forward (rx symbol-start (or "def" "class") symbol-end) nil t))
          (forward-line -1)
          (insert (format "\n__all__ = [\'%s\']\n" thing)))))))

(provide 'cozi)


