#!/bin/bash

nohup \
  /Applications/Emacs.app/Contents/MacOS/Emacs \
  ${HOME}/Dropbox/scratch.txt \
  ${HOME}/Dropbox/lists/todo/*/todo.txt \
  &> /dev/null &
