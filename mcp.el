;;; mcp.el --- Model Context Protocol server config -*- lexical-binding: t -*-

;; `mcp-server-lib' is the library for building Model Context Protocol
;; servers inside Emacs, so external clients (Claude Code, Cursor, etc.)
;; can call into this running Emacs over stdio.
;;
;; Bootstrap once per machine:
;;   M-x mcp-server-lib-install         ;; drops ~/.emacs.d/emacs-mcp-stdio.sh
;;
;; Register with Claude Code (or any MCP-aware client):
;;   claude mcp add -s user -t stdio emacs -- \
;;     ~/.emacs.d/emacs-mcp-stdio.sh --server-id=default
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
  :commands (mcp-server-lib-start
             mcp-server-lib-stop
             mcp-server-lib-install
             mcp-server-lib-describe-setup
             mcp-server-lib-show-metrics))

;; (use-package elisp-dev-mcp
;;   :after mcp-server-lib
;;   :config (elisp-dev-mcp-enable))

;; (use-package org-mcp
;;   :after mcp-server-lib
;;   :config (org-mcp-enable))

(provide 'mcp)

;;; mcp.el ends here
