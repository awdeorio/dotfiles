#!/bin/bash

nohup command emacs \
  ${HOME}/Dropbox/lists/todo/home/todo.txt \
  ${HOME}/Dropbox/lists/todo/work/todo.txt \
  ${HOME}/Dropbox/scratch.txt \
  &> /dev/null &
