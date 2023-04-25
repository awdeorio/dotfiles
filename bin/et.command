#!/bin/bash
#
# Open todo lists in Emacs
#
# Open multiple files without automatically activating the "Buffer List" split
# https://emacs.stackexchange.com/questions/36364/how-to-open-multiple-files-without-automatically-activating-the-buffer-list-sp

nohup command emacs \
  --eval '(add-hook (quote window-setup-hook) (function delete-other-windows))' \
  ${HOME}/Dropbox/scratch.txt \
  ${HOME}/Dropbox/lists/todo/work/todo.txt \
  ${HOME}/Dropbox/org/home.org \
   &> /dev/null &
