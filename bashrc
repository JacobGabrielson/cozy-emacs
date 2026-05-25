# -*- sh -*-

# Current kubernetes context for the prompt. Stat-guard first so sessions
# with no kube config never pay the cost of spawning kubectl per prompt.
function kube_ctx {
    [[ -n $KUBECONFIG || -r $HOME/.kube/config ]] || return
    command -v kubectl >/dev/null 2>&1 || return
    local ctx
    ctx=$(kubectl config current-context 2>/dev/null) || return
    [[ -n $ctx ]] && printf ' k8s:%s' "$ctx"
}

if [[ ! -z $INSIDE_EMACS ]]; then
    # show full path for dirtrack
    # :e is signal it's emacs, for dirtrack-list in ~/.emacs
    # kube_ctx goes after :e so it can't disturb dirtrack's path capture
    export PS1='\h:\w:\j:e$(kube_ctx)\$ '
    export EDITOR="emacsclient --alternate-editor=vi -s '$DISPLAY'"
else
    export PS1='\h:\W:\j$(kube_ctx)\$ '
fi

shopt -s cdspell
shopt -s cmdhist
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
shopt -s extglob
shopt -s histappend
shopt -s histverify
shopt -s histreedit
shopt -s nocaseglob

export HISTTIMEFORMAT="%h %d %H:%M:%S "
export HISTSIZE=100000
export HISTFILESIZE=100000
PROMPT_COMMAND='history -a'
export HISTIGNORE="ls:ps:history"

# https://stackabuse.com/managing-python-environments-with-direnv-and-pyenv/
if hash direnv 2>/dev/null; then
    eval "$(direnv hook bash)"
    mkdir -p $HOME/.config/direnv
fi

# https://github.com/akermu/emacs-libvterm/blob/master/README.md#shell-side-configuration
function vterm_printf() {
    if [ -n "$TMUX" ]; then
	# Tell tmux to pass the escape sequences through
	# (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
	printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
	# GNU screen (screen, screen-256color, screen-256color-bce)
	printf "\eP\e]%s\007\e\\" "$1"
    else
	printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
	vterm_printf "51;Evterm-clear-scrollback";
	tput clear;
    }
fi

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
  function rfrsh { :;  }
fi

rfrsh

# Tab completion for tmux. Ubuntu's tmux/bash-completion packages no longer
# ship one, so this builds the subcommand list dynamically from tmux itself
# and handles session/window targets for the commands that take them.
_tmux_complete() {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local cmd="${COMP_WORDS[1]}"

    if (( COMP_CWORD == 1 )); then
        local cmds
        cmds=$(tmux list-commands 2>/dev/null | awk '{print $1}')
        COMPREPLY=($(compgen -W "$cmds" -- "$cur"))
        return
    fi

    case "$cmd" in
        attach|attach-session|kill-session|switch-client|has-session|rename-session)
            local sessions
            sessions=$(tmux list-sessions -F '#{session_name}' 2>/dev/null)
            COMPREPLY=($(compgen -W "$sessions" -- "$cur"))
            ;;
        kill-window|select-window|rename-window|move-window|link-window|swap-window|select-pane|kill-pane)
            local windows
            windows=$(tmux list-windows -aF '#{session_name}:#{window_index}' 2>/dev/null)
            COMPREPLY=($(compgen -W "$windows" -- "$cur"))
            ;;
    esac
}
complete -F _tmux_complete tmux

