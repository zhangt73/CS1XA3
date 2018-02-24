#!/bin/bash
upToDate() {
  git status 
}

uncommittedChanges() {
  git diff > changes.log
}

todos() {
  grep -r --exclude={} "#TODO" > todo.log
  # reference from https://gist.github.com/a1phanumeric/5346170 
  # To find out how to use exclude flag with grep command 
}
:
