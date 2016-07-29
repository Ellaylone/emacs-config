#!/bin/bash

# Ask for path to emacs config file
# Defaults to ~/.emacs
emacs_path="~/.emacs"
askEmacsPath () {
    read -e -p "Enter path to .emacs($emacs_path): " path
}

# Ask for path to emacs user directory
# Defaults to ~/.emacs.d/
emacs_dir_path="~/.emacs.d/"
askEmacsDirPath () {
    read -e -p "Enter path to user-emacs-directory($emacs_dir_path): " path
}

# Check if emacs input is empty
checkEmacsPathInput () {
    if [[ "$path" != '' ]]
    then
        emacs_path=$path
    fi
}

# Check if emacs user directory input is empty
checkEmacsDirPathInput () {
    if [[ "$path" != '' ]]
    then
        emacs_dir_path=$path
    fi
}

# Check if input values are correct
checkIfCorrect () {
    while true; do
        read -p "Is it correct(Y/n)?" yn
        case $yn in
            [Yy]* ) make install; break;;
            [Nn]* ) exit;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}

main () {
    askEmacsPath
    checkEmacsPathInput
    askEmacsDirPath
    checkEmacsDirPathInput
    echo $emacs_path
    echo $emacs_dir_path
    checkIfCorrect
}

main
