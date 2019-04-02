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


