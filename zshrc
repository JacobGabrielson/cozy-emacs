# -*- shell-script -*-

# History
HISTFILE=$HOME/.zsh_history
SAVEHIST=7000
HISTSIZE=8000

setopt auto_pushd
setopt pushd_ignore_dups
setopt auto_list
setopt auto_menu
setopt append_history
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_no_functions
setopt hist_no_store
setopt hist_reduce_blanks
setopt hist_verify
setopt share_history
setopt no_flow_control
setopt interactive_comments
setopt print_exit_value
setopt short_loops
setopt auto_continue
setopt prompt_subst
bindkey -e

if [[ $(uname) = 'Darwin' ]]; then
  export HOMEBREW_VERBOSE=1
fi

# vcs_info: lightweight git status for the prompt
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:git:*' formats       ' (%b%u%c)'
zstyle ':vcs_info:git:*' actionformats ' (%b|%a%u%c)'
precmd() { vcs_info }

# Prompt. Two shapes: a quiet, dirtrack-friendly one inside Emacs shell-mode,
# and a colorful one with git info + exit code everywhere else.
if [[ -n $INSIDE_EMACS && $INSIDE_EMACS != *vterm* ]]; then
  # Trailing ':e' marks this as Emacs for dirtrack-list in vanilla.el / .emacs.
  PROMPT='%m:%~:%j:e%# '
  export EDITOR="emacsclient --alternate-editor=vi"
else
  # %F{...}/%f = color on/off, %(?..X) = X when last exit != 0
  PROMPT='%F{cyan}%n%f@%F{green}%m%f %F{yellow}%~%f%F{magenta}${vcs_info_msg_0_}%f%(1j. %F{blue}[%j]%f.)%(?.. %F{red}[%?]%f)
%# '
fi

# direnv (per-directory env vars)
if hash direnv 2>/dev/null; then
    eval "$(direnv hook zsh)"
    mkdir -p $HOME/.config/direnv
fi

# emacs-libvterm shell-side helpers
# https://github.com/akermu/emacs-libvterm/blob/master/README.md#shell-side-configuration
function vterm_printf() {
    if [ -n "$TMUX" ]; then
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    }
fi

# Pull a fresh SSH_AUTH_SOCK / DISPLAY out of tmux after reattach.
# From https://babushk.in/posts/renew-environment-tmux.html
if [[ -n $TMUX ]]; then
  function rfrsh {
    if [[ -n $SSH_AUTH_SOCK ]]; then
      export $(tmux show-environment | grep "^SSH_AUTH_SOCK")
    fi
    if [[ -n $DISPLAY ]]; then
      export $(tmux show-environment | grep "^DISPLAY")
    fi
  }
else
  function rfrsh { : ; }
fi

rfrsh
