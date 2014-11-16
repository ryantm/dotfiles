# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors ~/.dir_colors`"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi


alias bcd='rm -Rf *.elc;emacs -Q --eval "(progn (require '"'"'package)(setq load-path (cons \"`pwd`\" load-path))(byte-recompile-directory \"`pwd`\" 0 t)(check-declare-directory \"`pwd`\"))"'

alias dbc='rm -vRf *.elc'

