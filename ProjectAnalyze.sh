#!/bin/bash
clear
echo "Welcome To Project Analyzer. You are provided with some tools to analyze your current project. Enter the corresponding command to conduct each specific task. If you are not sure where to start with, try type HELP and help will be on your way :) "

# Question 1 : Inform if your local repo is up to date with remote repo
upToDate() {
  git status
}

# Question 2: Puts all uncommitted changes in changes.log
uncommittedChanges() {
  git diff > changes.log 
 # Check the diff using git diff command and direct the out put to changes.log
}

# Question 3: Put each line from every file of the project with tag TODO in todo.log

todos() {
  grep -r --exclude={todo.log,ProjectAnalyze.sh} "#TODO" > todo.log
  # Use grep command to filter the lines with TODO tags.
  # Use --exclude tag to avoid duplication 
  # reference from https://gist.github.com/a1phanumeric/5346170 
  # For how to use exclude flag with grep command 
}

# Question 4: Direct all syntax error results in Haskell files to error.log:
directErrors() {
  ghc -fno-code "*.hs" &> error.log
  # Use &> to direct errors into the target
}

# General control flow of the bash script
ACTION=NOTSET # Variable that holds the command user entered
# The ACTION variable is used to determine which task should be done by the script
while [ "$ACTION" != "quit" ] # Stop prompting when user type quit and exit  
do 
    read -p "Please enter the command in order to conduct analysis: " ACTION
    if [ "$ACTION" = "HELP" ] # Display help menu for users 
    then 
         helpMenu
    elif [ "$ACTION" = "state" ]
    then  
         upToDate
    elif [ "$ACTION" = "change" ]
    then
         uncommittedChanges
    elif [ "$ACTION" = "todo" ]
    then
         todos
    elif [ "$ACTION" = "error" ]
    then 
         directErrors
    elif [ "$ACTION" != "quit" ]
    then
         echo "Invalid Command. Please try another time"
        # In case the user entered invalid command
    fi 
done 

