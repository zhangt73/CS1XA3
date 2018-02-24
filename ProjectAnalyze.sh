#!/bin/bash
clear
echo "Welcome To Project Analyzer. You are provided with some tools to analyze your current project. Enter the corresponding command to conduct each specific task. If you are not sure where to start with, try type HELP and help will be on your way :) "

ACTION=NOTSET
while [ "$ACTION" != "quit" ] 
do 
    read -p "Please enter the command in order to conduct analysis: " ACTION
    if [ "$ACTION" = "HELP" ] 
    then 
         helpMenu
    elif [ "$ACTION" = "state" ]  
    then 
         upToDate
   elif [ "$ACTION" = "change" ]
    then
         uncommittedChanges
   elif [ "$ACTION" = "change" ]
    then
         uncommittedChanges
   elif [ "$ACTION" = "todo" ]
    then
         todos
   elif [ "$ACTION" = "error" ]
    then 
         directErrors
   else
         echo "Invalid Command. Please try another time"
done 

          

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
directErrors() {
  ghc -fno-code "*.hs" &> error.log
}

