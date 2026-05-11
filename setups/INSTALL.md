# Dev environment install checklist

A running list of what to install on a fresh machine to get this Emacs config
and the tools it expects working. Edit as the setup drifts.

## macOS

### Core

- [x] [Homebrew](https://brew.sh/)
- [ ] Emacs 31+ — `brew install --cask emacs-app-good`
      (from [emacs-builds](https://github.com/jimeh/emacs-builds))
- [ ] git
- [ ] tmux
- [ ] ripgrep (`brew install ripgrep`)
- [ ] fd (`brew install fd`)
- [ ] shellcheck (`brew install shellcheck`) — used by flycheck for shell
- [ ] gnupg
- [ ] direnv / envrc (`brew install direnv`)
- [x] coreutils (`brew install coreutils`) — 9.11. Provides GNU `gls` so
      dired can use `--dired`; vanilla.el wires this up automatically.

### Fonts / terminal

- [x] JetBrainsMono Nerd Font (`brew install --cask font-jetbrains-mono-nerd-font`).
      Used by Alacritty and Emacs default face; doom-modeline glyphs render
      inline because the Nerd Font carries the icon range. After install, also
      run `M-x nerd-icons-install-fonts` once in Emacs to drop the
      `Symbols Nerd Font Mono.ttf` into `~/Library/Fonts` for completeness.
- [ ] Consolas or another font with italic + 24-bit color support
- [x] iTerm2 (`brew install --cask iterm2`)
- [x] Alacritty (`brew install --cask alacritty`)
- [ ] Kitty (alternative)

## Linux

### System packages (Debian/Ubuntu)

- [ ] `build-essential ca-certificates curl autoconf texinfo`
- [ ] `libncurses5-dev libgnutls28-dev gnutls-dev gnutls-bin`
- [ ] `libx11-dev libgtk-3-dev libxpm-dev libjpeg-dev libtiff-dev libgif-dev`
- [ ] `libjansson-dev libsystemd-dev libotf-dev libgpm-dev libxml2-dev`
- [ ] `libcairo2-dev libmagickcore-dev liblcms2-dev librsvg2-dev`
      (for building Emacs from source — see `builds/emacs/Dockerfile`)
- [ ] ripgrep, fd, shellcheck, tmux, gnupg, direnv

## Language toolchains

### Go

- [x] Go (`brew install go`) — go 1.26.3
- [ ] gopls and friends — run `setups/install-go-tools.sh`
      (gocode, golint, godef, errcheck, godoc, gogetdoc, goimports,
      gorename, gomvpkg, guru, gopls)

### Rust

- [x] [rustup](https://rustup.rs/) (curl-bash install) — rustc 1.95.0 stable
- [x] Nightly toolchain — `rustup toolchain add nightly` (per `setups/rust.sh`)
- [ ] rust-analyzer (via `rustup component add rust-analyzer` or distro)
- [ ] racer (legacy; nightly toolchain required)

### Python

- [ ] python3 + pip
- [ ] [pyenv](https://github.com/pyenv/pyenv) (optional, for version mgmt)
- [ ] `pip install ipython` (used as `python-shell-interpreter`)
- [ ] `pip install pylint flake8 black`
- [ ] `pip install python-lsp-server` (or pyright via npm)

### Node / TypeScript

- [x] Node.js (`brew install node`) — node 26.0.0, npm 11.12.1
- [ ] `npm install -g typescript typescript-language-server`
- [ ] `npm install -g prettier eslint`

### Java

- [ ] JDK (Temurin via brew: `brew install --cask temurin`)
- [ ] Maven / Gradle as needed
- [ ] [eclipse.jdt.ls](https://github.com/eclipse-jdtls/eclipse.jdt.ls)
      (lsp-java auto-installs, but JDK must be on PATH)

### Clojure

- [ ] [clojure CLI](https://clojure.org/guides/install_clojure)
- [ ] [clojure-lsp](https://clojure-lsp.io/)
- [ ] leiningen (optional)

### Lisp / Scheme

- [x] sbcl (`brew install sbcl`) — 2.6.4. Used by SLIME.

### Haskell

- [x] [ghcup](https://www.haskell.org/ghcup/) (`brew install ghcup`) — 0.1.50.2.
      Use `ghcup install ghc` / `ghcup install cabal` / `ghcup install stack`
      to provision the actual toolchain.
- [ ] hindent, intero (legacy)

### Elm

- [ ] elm (`npm install -g elm`)

### C/C++

- [ ] clang, clangd (for irony / eglot)

## Infra / CLIs

- [x] Docker Desktop — primary container runtime on macOS. Local Kubernetes
      via **Settings → Kubernetes → Enable Kubernetes** (single-node;
      writes context `docker-desktop` into `~/.kube/config`). Allocate
      enough CPU/memory in Docker Desktop settings since k8s shares the VM.
- [x] [colima](https://github.com/abiosoft/colima) (`brew install colima`) — 0.10.1.
      Installed but unused while Docker Desktop is the active runtime.
      Keep around as a fallback if Docker Desktop licensing ever changes.
- [ ] kubectl
- [ ] [kind](https://kind.sigs.k8s.io/) — for multi-node local clusters on
      top of Docker Desktop (`brew install kind` then `kind create cluster`)
- [ ] terraform / opentofu (hcl-mode)
- [ ] aws-cli
- [ ] gh (GitHub CLI)
- [ ] ~~microk8s~~ — **skipped on macOS**: the `ubuntu/microk8s` Homebrew
      tap is broken on current Homebrew (uses removed `depends_on macos:`
      API). Docker Desktop's built-in Kubernetes covers the local-cluster
      use case; reach for kind on top of Docker Desktop if you need
      multi-node.

## AI / LLM tooling

- [x] [Ollama](https://ollama.com/) (`brew install ollama`) — 0.23.2.
      Metal-accelerated local LLM runtime on Apple Silicon. Run
      `ollama serve` to start the daemon (or `brew services start ollama`).
- [x] [opencode](https://opencode.ai/) (`brew install sst/tap/opencode`)
      — 1.14.41. Terminal coding agent; speaks to Ollama, Anthropic, and
      OpenAI providers.
- [x] **Local coding model:** `qwen2.5-coder:32b` via
      `ollama pull qwen2.5-coder:32b` (~20GB in 4-bit). Strong open-weights
      coding model in the 32B class; runs fast on M-series with 128GB
      unified memory.
- [x] **Reasoning-heavy alternative:** `deepseek-r1:70b` via
      `ollama pull deepseek-r1:70b` (~42GB). Llama-distilled DeepSeek-R1.
      Good for tricky algorithmic / debugging problems where chain-of-thought
      helps. Slower than qwen2.5-coder because it emits long reasoning
      traces before answering.

## Emacs packages

Most are auto-installed via `use-package` from MELPA on first launch. Notable
externals they shell out to:

- ripgrep (deadgrep, projectile)
- shellcheck (flycheck)
- LanguageTool jar (langtool.el)
- pdftotext / mupdf (pdf-tools)

## GUI / editors

- [x] [Zed](https://zed.dev/) (`brew install --cask zed`) — keymap in
      `zed/keymap.json`
- [ ] LanguageTool (for langtool.el)

## TeX / LaTeX

- [x] TeX Live (`brew install texlive`) — full distribution. Provides `tex`,
      `latex`, `pdflatex`, `xelatex`, `lualatex`, `bibtex`, `biber`, etc.
      Used by AUCTeX in Emacs.
