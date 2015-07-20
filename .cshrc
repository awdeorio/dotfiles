#!/bin/csh

# Automatically switch to bash if it isn't already the default shell,
# otherwise assume that bash is the default and we need csh for some
# reason

set default_shell=`grep ${USER} /etc/passwd | cut -d: -f7`
if ($?prompt && ( -x /bin/bash ) && $default_shell != "/bin/bash") then
   if (! $?comm) then
      set comm = `/bin/ps -fp $$ | tail -1 | awk '{ print $8 }'`
   endif

   if ( "$comm" == "-csh") then
      if ( -x "$HOME/.login" ) then
   source "$HOME/.login"
      endif
      exec /bin/bash --login
   else
      exec /bin/bash
   endif

endif

#set prompt='%n@%m %c $ '
set prompt='%{^[[00;36m%}%n@%m %{^[[01;34m%}%c $%{^[[00m%} '
setenv PAGER less
setenv EDITOR emacs
setenv LESS '--ignore-case'

# Aliases
unalias rm mv cp lp  # caen aliases these with -i
eval `dircolors --csh ~/.DIR_COLORS`
alias ls 'ls --color --ignore-backups'
alias ll 'ls -l --human-readable --ignore-backups'
alias cdd 'cd ..'

# shell preferences
unsetenv SHELL  # this may be left over from bash
set history = 2000
set savehist = 2000
set histdup = prev
set nobeep
set notify
unset {ignoreeof,autologout}

# Tab completion
set filec
set autolist
set color
set colorcat
set addsuffix
set complete = igncase #case-insensitive tab completion

# Key bindings
bindkey "^r" i-search-back
