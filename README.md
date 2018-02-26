# Project Analyze Script
---
## Overview
The following is the documentation for the bash script assignment for CS1XA3 at McMaster University. The script demonstrate a list of functionalities that could be used to conduct analysis for your projects. When you start running the script, it will ask for your instructions. Each command you type correspond to one task to be done. In next section, I will go over all the functionalities and show how they could be used.
---
## state
This function is used to check if your local repo is up to date with the remote on github. It will show you the current status of your local repo.

## change
This function will put all uncommitted changes of your local repo into file changes.log where you can view all of them at once.

## todo
This function will put each line from every file of the project with tag #TODO in todo.log where you can view all of them at once.

## error
This function will look up all Haskell files in your current directory and put all run time syntax error into error.log

## list
This extra feature of the script allow users to check the biggest file/dir in terms of how much space they occupy. The user could customize the tool to specify how many files/dir they want to see in the list. The default should be 10(when actual files/dir exceed 10)

## quit
If you get bored, simply type quit and the script will exit immediately

## HELP

If you get confused, simply call HELP and help menu will show up to guide you how to use this script.


