;;; mcp.el --- Model Context Protocol server config -*- lexical-binding: t -*-

;; `mcp-server-lib' is the library for building Model Context Protocol
;; servers inside Emacs, so external clients (Claude Code, Cursor, etc.)
;; can call into this running Emacs over stdio.
;;
;; Bootstrap once per machine:
;;   M-x mcp-server-lib-install         ;; drops ~/.emacs.d/emacs-mcp-stdio.sh
;;
;; Register with Claude Code (or any MCP-aware client). NOTE: --server-id
;; must match the id the provider registers its tools under, NOT "default".
;; elisp-dev-mcp (below) registers under "elisp-dev-mcp"; pointing a client
;; at "default" connects fine but exposes zero tools.
;;   claude mcp add -s user -t stdio emacs -- \
;;     ~/.emacs.d/emacs-mcp-stdio.sh --server-id=elisp-dev-mcp
;;
;; Useful commands:
;;   M-x mcp-server-lib-start           ;; start the server
;;   M-x mcp-server-lib-stop            ;; stop the server
;;   M-x mcp-server-lib-describe-setup  ;; list tools/resources + metrics
;;   M-x mcp-server-lib-show-metrics    ;; usage counters
;;
;; The library by itself exposes nothing — you also need at least one
;; package that *registers* tools/resources via the API. Two prebuilt
;; ones from the same author live on MELPA:
;;
;;   elisp-dev-mcp  - elisp introspection / development tools
;;   org-mcp        - read & write Org content
;;
;; To enable them, uncomment the corresponding use-package blocks below.

(use-package mcp-server-lib
  ;; Load eagerly. External clients connect on their own schedule, so the
  ;; server must be up from startup — there's no in-Emacs command they can
  ;; trigger to fault in an autoload.
  :demand t
  :config
  ;; The stdio bridge (emacs-mcp-stdio.sh) talks to us through `emacsclient',
  ;; so the Emacs server has to be listening or every request fails with
  ;; "can't find socket".
  (require 'server)
  (unless (server-running-p)
    (server-start))
  ;; `mcp-server-lib-process-jsonrpc' (called per request by the bridge)
  ;; refuses to serve until the server is "started" (it just flips an internal
  ;; running flag and resets metrics). It errors if started twice, so guard
  ;; for config reloads.
  (unless (bound-and-true-p mcp-server-lib--running)
    (mcp-server-lib-start)))

(use-package elisp-dev-mcp
  :after mcp-server-lib
  :demand t
  :config (elisp-dev-mcp-enable))

;; (use-package org-mcp
;;   :after mcp-server-lib
;;   :config (org-mcp-enable))

(provide 'mcp)

;;; mcp.el ends here
