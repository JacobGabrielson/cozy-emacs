# -*- shell-script -*-

[[ -z ${PS1} ]] && return

if [[ ! -z $INSIDE_EMACS ]]; then
    # show full path for dirtrack
    # :e is signal it's emacs, for dirtrack-list in ~/.emacs
    export PS1='\h:\w:\j:e\$ '
    export EDITOR="emacsclient --alternate-editor=vi -s '$DISPLAY'"
else
    export PS1='\h:\W:\j\$ '
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

export PATH=$HOME/.local/bin:$PATH

if hash pyenv 2>/dev/null; then 
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

if hash direnv 2>/dev/null; then
    eval "$(direnv hook bash)"
fi

export PATH=$HOME/Documents/GitHub/chaws/bin:$PATH

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

export MAKEFLAGS='-j 16'

export PATH="$HOME/.local/go/bin:$HOME/go/bin:$PATH"
if [[ -f $HOME/.cargo/env ]]; then
    . $HOME/.cargo/env
fi


# From https://babushk.in/posts/renew-environment-tmux.html
if [[ -n $TMUX ]]; then                                                                               
  function rfrsh {                                                                                
    export $(tmux show-environment | grep "^SSH_AUTH_SOCK")
    export $(tmux show-environment | grep "^DISPLAY")
  }
else                                                                                                  
  function rfrsh { :;  }
fi

rfrsh


