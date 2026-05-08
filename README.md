# cozy-emacs

Jacob's personal Emacs configuration, plus a handful of related dotfiles
(shell, tmux, Xresources, Zed keymap) that travel with it.

## Usage

Clone the repo somewhere, then add to `~/.emacs`:

```elisp
(add-to-list 'load-path "/path/to/cozy-emacs")
(load "vanilla")
(load "packages")
(load "programming")
;; ...load whichever modules you want
(load "last") ; always load this last
```

`last.el` is meant to be loaded after everything else — it sets up
`envrc-global-mode`, which needs to wrap other modes' hooks.

## Layout

### Core Emacs config

- `vanilla.el` — basic customizations using only built-in Emacs (mode toggles,
  display, key bindings). Targets Emacs 27+.
- `packages.el` — third-party packages installed via `use-package` from MELPA.
- `packages-extra.el` — optional extras.
- `programming.el` — general programming-mode tweaks.
- `cozy.el` — older legacy config (Erlang/distel, JS, Python, w3m). Predates
  the `vanilla.el`/`packages.el` split.
- `last.el` — load this last; sets up `envrc`.

### Language-specific

- `common-lsp.el` — shared LSP / `eglot` setup.
- `clj-lsp.el`, `go-lsp.el`, `go-eglot.el`, `java-lsp.el`, `python-lsp.el`,
  `rust-lsp.el`, `ts-lsp.el` — per-language LSP config.
- `elisp-setup.el`, `lisp.el` — Lisp editing.
- `kubernetes.el`, `langtool.el`, `snippets.el` — assorted modes.

### Other dotfiles

- `bashrc`, `bash_profile`, `zshrc` — shell config.
- `tmux.conf`, `Xresources`, `sqliterc` — terminal / X / sqlite.
- `alacritty.toml` — Alacritty config; symlinked from
  `~/.config/alacritty/alacritty.toml`.
- `zed/keymap.json` — Zed editor keybindings.
- `epylint`, `flyalyzer` — helper scripts.

### Setup helpers

- `setups/INSTALL.md` — checklist of everything I install on a fresh dev
  machine to get this config working. Keep it up to date.
- `setups/install-go-tools.sh` — install Go tooling for LSP.
- `setups/rust.sh` — Rust toolchain setup.
- `setups/run.el` — ad-hoc run commands.
- `setups/windows-terminal/` — Windows Terminal config notes.
- `builds/emacs/Dockerfile` — build Emacs 27 from source in Ubuntu 18.04.

## Notes

- Aim for Emacs 31 or newer.
- On macOS, install Emacs via [emacs-builds](https://github.com/jimeh/emacs-builds)
  (`brew install --cask emacs-app-good`).
- For 24-bit color and italics in a terminal on Windows, Kitty with Consolas
  is the only combo that has worked reliably.
