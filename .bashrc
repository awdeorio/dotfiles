#!/bin/bash

###############################################################################
#
#  ~/.bashrc
#
#  Drew's Sweet Bashrc
#
###############################################################################


### places ####################################################################
export NFSHOME=/net/trenton/w/awdeorio
export IFSHOME=/afs/umich.edu/user/a/w/awdeorio
export WWWHOME=/net/web/w/web/u/a/awdeorio
export WWWINFERNO=/net/web/w/web/inferno
export BACKUP=${NFSHOME}/backup/nacho/awdeorio
export EECS280=/afs/umich.edu/class/eecs280

# set umask for both scp and ssh
umask 002


### Aliases ###################################################################
alias du="du -sh"
alias dusort="command du -s * .* | sort -n"
alias df="df -h"
alias cdd="cd .."
alias grep="grep --color"
alias igrep="grep -i"
alias wcl="wc -l"
alias rmb='rm -vf *~'
alias latex='latex -halt-on-error'
alias lftps='lftp -e "open -u awdeorio,xx sftp://snoopy.eecs.umich.edu"'
alias dftp='ssh -R 19999:localhost:22'
function dftp-get { command scp -r -P19999 "$@" localhost: ; }
alias shred='shred --remove'
alias R='R --quiet --no-save'
#NOTE: see later for ls options
alias xat='xattr -r -d com.apple.quarantine /Users/awdeorio/mnt/finance'


### Editor ####################################################################
alias e="emacs"
export EDITOR='emacs'                # my favorite editor
function emacs {
  # Make emacs start in the background, change window title
  if [ "$1" == "-nw" ]; then
    command emacs "$@"
    return
  elif [ `uname` = "Darwin" ]; then
    /Applications/Emacs.app/Contents/MacOS/Emacs "$@" &
  elif [ "$DISPLAY" ] || [ "$OS" = "Windows_NT" ]; then
    bgui emacs "$@" &
  else
    # for console
    command emacs "$@"
  fi
}


### Pager #####################################################################
alias more=less
alias less="less --shift 5 --ignore-case --chop-long-lines --RAW-CONTROL-CHARS --LONG-PROMPT"
export PAGER=less
export LESSOPEN="|${HOME}/bin/lesspipe.sh %s"


### Path stuff ################################################################
# remove item from $PATH
path-remove () {
  local IFS=':'
  local NEWPATH
  for DIR in $PATH; do
    if [ "$DIR" != "$1" ]; then
      NEWPATH=${NEWPATH:+$NEWPATH:}$DIR
    fi
  done
  export PATH=${NEWPATH};
}

# add item to end of $PATH, uniquely
path-append () {
  [ -d $1 ] || return 1    # make sure directory exists
  path-remove $1           # remove the directory
  export PATH=${PATH}:${1} # append the directory
}

# add item to beginning of $PATH, uniquely
path-prepend () {
  [ -d $1 ] || return 1     # make sure directory exists    
  path-remove $1            # remove the directory
  export PATH=${1}:${PATH}  # append the directory
}

path-append /usr/local/bin
path-append /usr/local/sbin
path-append /usr/bin
path-append /usr/sbin
path-append /bin
path-append /sbin
path-prepend ${HOME}/bin
path-append ${HOME}/local/bin
path-append ${HOME}/local/sbin
path-append ${HOME}/.rvm/bin   # Add RVM to PATH for scripting
if [ `whoami` != "root" ]; then
  path-append /usr/caen/bin
  path-append /usr/um/bin
fi


### local tool installs ########################################################
[ -d ${HOME}/local/lib ]     && export LIBARY_PATH=${LIBRARY_PATH}:${HOME}/local/lib
[ -d ${HOME}/local/lib ]     && export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HOME}/local/lib
[ -d ${HOME}/local/lib ]     && export LD_RUN_PATH=${LD_RUN_PATH}:${HOME}/local/lib
[ -d ${HOME}/local/include ] && export CPATH=${CPATH}:${HOME}/local/include
[ -d ${CPATH} ]              && export C_INCLUDE_PATH=${C_INCLUDE_PATH}:${CPATH}
[ -d ${CPATH} ]              && export CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH}:${CPATH}
[ -d ${HOME}/local/man ]     && export MANPATH=${HOME}/local/man:${MANPATH}

# OS X GNU Coreutils
path-prepend /usr/local/opt/coreutils/libexec/gnubin
[ -d /usr/local/opt/coreutils/libexec/gnuman ] && export MATHPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH

# local perl module installs
if [ -d ${HOME}/local/lib/perl5 ]; then
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib64/perl5/site_perl/5.8.8/x86_64-linux-thread-multi
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib/perl5/site_perl/5.8.8
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib/perl5/site_perl
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib64/perl5/vendor_perl/5.8.8/x86_64-linux-thread-multi
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib/perl5/vendor_perl/5.8.8
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib/perl5/vendor_perl
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib64/perl5/5.8.8/x86_64-linux-thread-multi
  PERL5LIB=${PERL5LIB}:${HOME}/local/lib/perl5/5.8.8
  export PERL5LIB;
fi

# local python module installs
if [ -d ${HOME}/local/lib/python2.6 ]; then
  export PYTHONPATH=${HOME}/local/lib/python2.6:${PYTHONPATH}
  export PYTHONUSERBASE=${HOME}/local
fi
export PYTHONSTARTUP=~/.pythonrc.py

# local Ruby
# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# CCache
path-prepend /usr/lib/ccache/bin || path-prepend /usr/lib/ccache


###############################################################################
### source local file, if it's there
[ -f ~/.bashrc-local ] && source ~/.bashrc-local


################################################################################
# Test for an interactive shell.  There is no need to set anything past this 
# point for scp and rcp, and it's important to refrain from outputting 
# anything in those cases.
[[ $- != *i* ]] && return



### Make Certain Applications Start in the Background #########################
function bgui {
# test for GUI and application, start it in the background
# Usage: textx_gui(application_name)

  EXE="$1"
  EXE_FULLPATH=`/usr/bin/which $EXE 2> /dev/null`
    #echo "exe = $EXE, exe_fullpath = $EXE_FULLPATH"

  if [ ! "$DISPLAY" ] && [ ! "$OS" = "Windows_NT" ]; then
    echo "bgui Error: No display found."
    return 1
  fi
  if [ ! "$EXE_FULLPATH" ]; then
    echo "bgui Error: Can't find path to executable $EXE."
    return 1
  fi
  if ! test -x "$EXE_FULLPATH"; then
    echo "bgui Error: $EXE_FULLPATH is not executable"
    return 1
  fi

    # note: $@ already contains the command itself
  command nohup "$@" &> /dev/null &
}

# alias skype='bgui skype'
# alias thunderbird='bgui thunderbird'
# alias evince='bgui evince'
# alias acroread='bgui acroread'
# alias meld='bgui meld'
# alias firefox='bgui firefox'
# alias google-chrome='bgui google-chrome'
# alias soffice='bgui soffice'
# alias rhythmbox='bgui rhythmbox'
# alias eog='bgui eog'
# alias virtualbox='bgui virtualbox'
# alias eclipse='bgui eclipse'
# alias pithos='bgui pithos'
# alias radiotray='bgui radiotray'


### Utility Functions #########################################################

# recursively grep for string
alias gg='grep -r . --binary-files=without-match --exclude-dir ".svn" --exclude "*~" -e'

# grep for email addresses
alias ge='egrep -o "\w+([._-]\w)*@\w+([._-]\w)*\.\w{2,4}" -e'

# Find a file with a pattern in name:
function ff() { 
  if [ `uname` = "Darwin" ]; then
    mdfind -onlyin . -name '*'$*'*' ;
  else
    find . -type f -iwholename '*'$*'*' ;
  fi
}

# Find a file with pattern $1 in name and Execute $2 on it:
function fe() {
  find . -type f -iname '*'$1'*' -exec "${2:-file}" {} \;  ;
}

# Alias for locate on OSX
if [ `uname` = "Darwin" ]; then
  alias locate='mdfind -name'
fi


# check if a process is running
function psg() {
  ps ax | grep "$1" | grep -v grep
}

# auto ping
function ping {
  if [ ! "$1" ]; then
    command ping google.com -c3
  else
    command ping "$@"
  fi
}

# source bashrc
function sb {
  BASHRC_FILE=$HOME/.bashrc
  echo "source $BASHRC_FILE"
  source $BASHRC_FILE
}


### Printing ##################################################################
# enscript --margins=left:right:top:bottom in postscript points
# the following gives L=R=T=B=1in
export ENSCRIPT='--media=Letter --word-wrap --margins=72:72:72:72'


### Prompt Look and Feel ######################################################
set -o emacs                         # emacs commandline mode
set -o history                       # enable up-arrow command history
export HISTIGNORE="&:ls:cd:bg:fg:ll" # ignore these commands in history
export HISTCONTROL="ignoredups"      # ignore duplicates in history
export FIGNORE="~"                   # don't show these prefixes in tab-comp
shopt -s cdspell                     # Allow shitty spelling in cd commands
shopt -s checkwinsize                # keep LINES and COLUMNS up to date
shopt -s cdable_vars                 # shell vars added cd expansion

# Fancy Prompt
if [ "$LOGNAME" == "root" ]; then      # r00t
  export PS1='\[\033[01;31m\]\u@\h \[\033[01;34m\]\W $ \[\033[00m\]'
elif [ "$SSH_CONNECTION" ]; then     # remote machines
  export PS1='\[\033[00;36m\]\u@\h \[\033[01;34m\]\W \$ \[\033[00m\]'
else                                 # local machine
  export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
fi

# Change the window title of X terminals
case $TERM in
  xterm*|rxvt|Eterm|eterm)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
    ;;
  screen)
    PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
    ;;
esac

# Colorized output
if [ `uname` = "Linux" ] || [ `uname` = "Darwin" ] && [ `which gls` ]; then
  # GNU ls
  if [ -f /etc/DIR_COLORS ]; then
    eval `dircolors -b /etc/DIR_COLORS`
  fi
  alias ls="gls --color --human-readable --ignore-backups"
  alias ll="gls --color --human-readable --ignore-backups -l"
  alias la="gls --color --human-readable -A"
else
  alias ls="ls -G -h"
  alias ll="ls -G -h -l"
  alias la="ls -G -h -A"
  export CLICOLOR=1
fi


### Bash-completion ###########################################################
if [ -f ~/.bash_completion ] && ! shopt -oq posix; then
  source ~/.bash_completion
fi


### Todotxt setup #############################################################
export TODO_DIR=${HOME}/Dropbox/lists/todo/work
alias t='todo.sh'
alias et='e ${HOME}/Dropbox/lists/todo/*/todo.txt'

# todo toggle
# switches default todo list
function tt {
  TODO_DIRS=(`ls -d Dropbox/lists/todo/*/`)
  NEW_DIR_IDX=0
  if [ "$TODO_DIR" == "${TODO_DIRS[$NEW_DIR_IDX]}" ]; then
    NEW_DIR_IDX=1;
  fi
  echo "TODO_DIR=${TODO_DIRS[$NEW_DIR_IDX]}"
  export TODO_DIR="${TODO_DIRS[$NEW_DIR_IDX]}";
}


# Clear History at the very end
history -c
