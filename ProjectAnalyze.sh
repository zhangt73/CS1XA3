#!/bin/bash
upToDate() {
  git status 
}

uncommittedChanges() {
  git diff > changes.log
}
