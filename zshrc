# -*- shell-script -*-

# Notes:
#   See https://blog.bigoodyssey.com/how-to-manage-multiple-java-version-in-macos-e5421345f6d0


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
bindkey -e

HISTFILE=$HOME/.zsh_history
SAVEHIST=7000
HISTSIZE=8000

if [[ ! -z $INSIDE_EMACS ]]; then
    
fi

if [[ $(uname) = 'Darwin' ]]; then
  export HOMEBREW_VERBOSE=1
fi
