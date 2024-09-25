#!/bin/bash
#
# .bashrc
#
# Andrew DeOrio's Bash customizations
# awdeorio@umich.edu


### Non-interactive shells ####################################################
if [ $TERM = tramp ]; then
  unset RPROMPT
  unset RPS1
  PS1="$ "
  unsetopt zle
  unsetopt rcs  # Inhibit loading of further config files
  return
fi

# Umask for both scp and ssh
umask 002

# Language
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

### Aliases ###################################################################
# NOTE: emacs, git, ls, and less  aliases appear later
alias du="du -sh"
function dusort {
  [[ -z "$@" ]] && FINDPATH="." || FINDPATH="$@"
  find $FINDPATH -mindepth 1 -maxdepth 1 -print0 | xargs -0 du -sh | sort -h ;
}
alias df="df -h"
alias cdd="cd .."
alias grep="grep --color"
alias egrep="egrep --color"
alias fgrep="fgrep --color"
alias zgrep="zgrep --color"
alias igrep="grep -i"
alias wcl="wc -l"
alias rmb='echo "rm -vf *~ .*~" && rm -vf *~ .*~'
alias rmt='[ -d ${HOME}/.Trash/ ] && echo "rm -rvf ${HOME}/.Trash/*" && rm -rvf ${HOME}/.Trash/*'
alias rmd='[ -d ${HOME}/Downloads/ ] && echo "rm -rvf ${HOME}/Downloads/*" && rm -rvf ${HOME}/Downloads/*'
alias rme='[ -d ${HOME}/Desktop/ ] && echo "rm -rvf ${HOME}/Desktop/*" && rm -rvf ${HOME}/Desktop/*'
alias rms='[ -d ${HOME}/.ssh/ ] && echo "rm -rvf ${HOME}/.ssh/socket-*" && rm -rvf ${HOME}/.ssh/socket-*'
alias latex='latex -halt-on-error'
alias dftp='ssh -R 19999:localhost:22'
function dftp-get { command scp -r -P19999 "$@" localhost: ; }
alias R='R --quiet --no-save'
alias grip='grip --norefresh --browser'
alias whatismyip='curl ipinfo.io/ip'
alias weather='curl http://wttr.in/ann_arbor?Tn1'
alias weather3='curl http://wttr.in/ann_arbor?Tn | less'
alias vboxmanage=VBoxManage
alias gg='grep -r . --binary-files=without-match --exclude-dir ".git" --exclude "*~" -e'
function ff() {
  find . \
       -iwholename '*'$*'*' \
       -not -iwholename '*/env/*' \
       -not -iwholename '*/venv/*' \
       -not -iwholename '*/node_modules/*' \
       -not -iwholename '*/__pycache__*' \
       -not -iwholename '*/tmp*' \
       -not -iwholename '*.cache*' \
       -not -path '*/\.*' \
    ;
}
alias fb="find . -name '*~'"
alias fbrm="find . -name '*~' -exec rm -v {} \;"
alias pylint='pylint --output-format=colorized'
alias gs='git status'
alias gd='git diff'
alias gr='git rebase'
alias gf='git fetch -p'
alias gb='git branch'
alias ag='ag --ignore "*bundle.js"'
alias phs='python3 -m http.server --bind localhost 8000'
alias bejs='bundle exec jekyll serve --host localhost --port 4000'
alias mogrify-1024='mogrify -resize 1024x1024'
alias pg='ping google.com -c3'
alias sb="source ~/.bashrc"

### macOS #####################################################################
# Homebrew configuration for x86_64 and arm64
if [ -e /opt/homebrew ]; then
  export HOMEBREW_PREFIX=/opt/homebrew
elif [ -e /usr/local/Homebrew ]; then
  export HOMEBREW_PREFIX=/usr/local
fi
if [ -e ${HOMEBREW_PREFIX}/bin/brew ]; then
  export HOMEBREW_NO_AUTO_UPDATE=1
  eval $(${HOMEBREW_PREFIX}/bin/brew shellenv)
fi

if which gfind &> /dev/null; then
  alias find='gfind'
fi
if test -d /Applications/Google\ Chrome.app; then
  alias chrome='open -a "Google Chrome" --args'
  alias google-chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'
fi
if test -d /Applications/Brave\ Browser.app; then
  alias brave='open -a "Brave Browser" --args'
  alias brave-private='brave --incognito'
fi
if test -d /Applications/Firefox.app; then
  alias firefox='open -a Firefox --args'
  alias firefox-private='open -a Firefox -n --args --private-window'
  alias firefox-new-window='open -a Firefox -n --args --new-window'
fi
if [ `uname -s` = "Darwin" ]; then
  alias meld='rm -rvf "${HOME}/.local/share/meld" "${HOME}/Library/Preferences/org.gnome.meld.plist" "${HOME}/Library/Saved Application State/org.gnome.meld.savedState/" && meld'
fi
if test -d /Applications/Microsoft\ Excel.app; then
  alias excel='open -a Microsoft\ Excel'
fi

# diff-highlight ships with Git
# DIFF_HIGHLIGHT=$(locate diff-highlight | grep 'diff-highlight/diff-highlight$' | head -n1)
# if [ -n "$DIFF_HIGHLIGHT" ]; then
#   alias diff-highlight="$DIFF_HIGHLIGHT"
# fi


### Editor ####################################################################
# export EDITOR="emacsclient -n -c"      # Open a new window w/ existing daemon
export EDITOR="emacs"
export VISUAL="$EDITOR"                # Here for historical reasons
export SUDO_EDITOR="emacs -nw -Q"      # Editor used by sudoedit and sudo -e
export GIT_EDITOR="emacs -nw -Q"       # Editor used by git commit
function e {
  # Start Emacs in the background unless we're on an SSH connection
  if [ "$SSH_CONNECTION" ]; then
    emacs -Q "$@"
  else
    emacs "$@" &
  fi
}
function ediff { emacs --eval "(ediff-files \"$1\" \"$2\")" & }

### Pager #####################################################################
export PAGER="less --chop-long-lines"
alias less="${PAGER}"
export LESSOPEN="| lesspipe.sh %s"


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
path-append ${HOMEBREW_PREFIX}/bin
path-append ${HOMEBREW_PREFIX}/sbin
path-append /usr/bin
path-append /usr/sbin
path-append /bin
path-append /sbin
path-prepend ${HOME}/bin
path-prepend ${HOME}/local/bin
path-prepend ${HOME}/local/sbin
path-prepend ${HOME}/.local/bin
path-prepend ${HOME}/.local/sbin
path-append /usr/caen/bin
path-append /usr/um/bin


### local tool installs ########################################################
[ -d ${HOME}/local/lib ]     && export LIBARY_PATH=${LIBRARY_PATH}:${HOME}/local/lib
[ -d ${HOME}/local/lib ]     && export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HOME}/local/lib
[ -d ${HOME}/local/lib ]     && export LD_RUN_PATH=${LD_RUN_PATH}:${HOME}/local/lib
[ -d ${HOME}/local/include ] && export CPATH=${CPATH}:${HOME}/local/include
[ -d ${CPATH} ]              && export C_INCLUDE_PATH=${C_INCLUDE_PATH}:${CPATH}
[ -d ${CPATH} ]              && export CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH}:${CPATH}
[ -d ${HOME}/local/man ]     && export MANPATH=${HOME}/local/man:${MANPATH}

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

# Python
# pipx packages install package
path-append "${HOME}/.local/bin"
MANPATH=${MANPATH}:${HOME}/.local/share/man
# path-append ${HOME}/.pyenv/shims
# for DIR in "$(ls -d /usr/local/opt/python@*/bin)"; do
#   path-append "${DIR}"
# done
# if command -v pyenv 1>/dev/null 2>&1; then
#   eval "$(pyenv init -)"
# fi

# Pytest bash completion
# https://docs.pytest.org/en/latest/bash-completion.html
# $ pipx install argcomplete
if type -a register-python-argcomplete &> /dev/null; then
  eval "$(register-python-argcomplete pytest)"
fi

# SQLite3 on macOS
if [ -d /usr/local/opt/sqlite/bin ]; then
  path-append /usr/local/opt/sqlite/bin
fi

# CCache
path-prepend /usr/lib/ccache/bin || path-prepend /usr/lib/ccache

# Go (golang)
export GOPATH=${HOME}/.go
if type -a go &> /dev/null; then
  path-append ${GOPATH}/bin
  path-append $(go env GOROOT)/bin
fi

# Ruby
# export GEM_HOME=${HOME}/.gem
# path-append ${GEM_HOME}/bin
# path-prepend ${HOMEBREW_PREFIX}/opt/ruby@3.1/bin
path-prepend ${HOMEBREW_PREFIX}/opt/ruby/bin
# path-prepend ${HOMEBREW_PREFIX}/lib/ruby/gems/3.3.0/bin
if which gem &> /dev/null; then
    GEM_BIN=$(gem env | grep 'EXECUTABLE DIRECTORY' | awk '{print $NF}')
    path-append ${GEM_BIN}
fi

# Java
path-prepend ${HOMEBREW_PREFIX}/opt/openjdk/bin

# Docker
path-append ${HOME}/.docker/bin

################################################################################
# Test for an interactive shell.  There is no need to set anything past this
# point for scp and rcp, and it's important to refrain from outputting anything
# in those cases.
[[ $- != *i* ]] && return


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
shopt -s checkwinsize                # keep LINES and COLUMNS up to date

function find_git_context() {
  # Based on https://github.com/jimeh/git-aware-prompt

  # Branch
  local BRANCH
  local GIT_BRANCH
  if BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
    if [[ "$branch" == "HEAD" ]]; then
      BRANCH='detached*'
    fi
    GIT_BRANCH="$BRANCH"
  else
    GIT_BRANCH=""
  fi

  # '*' for dirty
  local STATUS=$(git status --porcelain 2> /dev/null)
  local GIT_DIRTY
  if [[ "$STATUS" != "" ]]; then
    GIT_DIRTY='*'
  else
    GIT_DIRTY=''
  fi

  # Concatenate
  GIT_CONTEXT="${GIT_BRANCH}${GIT_DIRTY}"
}

function ps1_context {
  # For any of these bits of context that exist, display them and append
  # a space.  Ref: https://gist.github.com/datagrok/2199506
  VIRTUAL_ENV_BASE=`basename "$VIRTUAL_ENV"`
  find_git_context
  for v in "${GIT_CONTEXT}" \
             "${debian_chroot}" \
             "${VIRTUAL_ENV_BASE}" \
             "${GIT_DIRTY}" \
             "${PS1_CONTEXT}"; do
    echo -n "${v:+$v }"
  done
}

# Fancy Prompt
source ~/.bashrc_colors
case "$TERM" in
  xterm*|rxvt*|Eterm*|eterm*|screen*)
    # If the terminal supports colors, then use fancy terminal
    if [ "$LOGNAME" == "root" ]; then
      # root
      PS1='\[${bldred}\]\]\u@\h \[${bldblue}\]\W\n\$ \[${txtrst}\]'
    elif [[ "$HOSTNAME" == *"caen"* ]]; then
      # CAEN Linux is slow, don't use git context
      PS1='\[${txtblk}\]\[${bldcyn}\]\u@\h \[${bldblu}\]\W\n\$ \[${txtrst}\]'
    elif [ "$SSH_CONNECTION" ]; then
      # remote machines
      PS1='\[${txtblk}\]$(ps1_context)\[${bldcyn}\]\u@\h \[${bldblu}\]\W\n\$ \[${txtrst}\]'
    else
      # local machine
      PS1='\[${txtpur}\]$(ps1_context)\[${bldgrn}\]\u@\h \[${bldblu}\]\W\n\$ \[${txtrst}\]'
    fi
    ;;
  *)
    # Default no color, no fanciness
    PS1='$ '
    ;;
esac
export PS1

# Change the window title of X terminals
case "$TERM" in
  xterm*|rxvt*|Eterm*|eterm*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
    ;;
  screen*)
    PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
    ;;
esac
export PROMPT_COMMAND

# Colorized output
LS=ls
if which gls &> /dev/null; then
  # GNU ls on OSX
  LS=gls
fi
if `${LS} --version 2>&1 | grep -q GNU &> /dev/null`; then
  # GNU ls
  # eval `dircolors -b ${HOME}/.DIR_COLORS`
  LSOPT="--color=auto --human-readable --quoting-style=literal --ignore-backups --ignore $'Icon\r'"
else
  # BSD ls
  # -G is for color
  LSOPT="-G"
fi
alias ls="${LS} -h ${LSOPT}"
alias ll="${LS} -h -l ${LSOPT}"
alias la="${LS} -h -A ${LSOPT}"


### Bash-completion ###########################################################
if which brew &>/dev/null && [[ -f $(brew --prefix)/etc/bash_completion ]]; then
  # OS X
  . $(brew --prefix)/etc/bash_completion
elif [[ $PS1 && -f /usr/share/bash-completion/bash_completion ]]; then
  . /usr/share/bash-completion/bash_completion
fi
for F in `find ${HOME}/.bash_completion.d/ -type f`; do
  source $F
done


### Disable warning default shell xsh on macOS Catalina and higher
if [ `uname -s` = "Darwin" ]; then
  export BASH_SILENCE_DEPRECATION_WARNING=1
fi


### Git customization #########################################################
# Alias "g" to "git" and don't break bash completion
alias g=git
complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
  || complete -o default -o nospace -F _git g

# Clear History at the very end
history -c
