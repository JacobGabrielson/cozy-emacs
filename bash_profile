# -*- shell-script -*-

export PATH=$HOME/.local/bin:$PATH
export MAKEFLAGS='-j 16'
export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"
if [[ -f $HOME/.cargo/env ]]; then
    . $HOME/.cargo/env
fi

# https://geeksocket.in/posts/emacs-lsp-go/
export GOPATH="$(go env GOPATH)"
export PATH="${PATH}:${GOPATH}/bin"

