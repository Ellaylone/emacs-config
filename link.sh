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

# https://gist.github.com/davejamesmiller/1965569
ask() {
    # http://djm.me/ask
    while true; do

        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo -n "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read REPLY </dev/tty

        # Default?
        if [ -z "$REPLY" ]; then
            REPLY=$default
        fi

        # Check if the reply is valid
        case "$REPLY" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}

# Check if input values are correct
checkIfCorrect () {
    echo $emacs_path
    echo $emacs_dir_path
    if ask "Is this correct?" Y; then
        echo "Finished"
    else
        exit
    fi
}

userInput () {
    askEmacsPath
    checkEmacsPathInput
    askEmacsDirPath
    checkEmacsDirPathInput
    checkIfCorrect
}

main () {
    userInput
    exit 0
}

main
