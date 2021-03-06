#!/bin/bash
set -e

current=${PWD}
current_emacs="$current/.emacs"
current_emacs_modules="$current/modules/"
current_emacs_snippets="$current/snippets/"
emacs_path="$HOME/.emacs"
emacs_dir_path="$HOME/.emacs.d/"
emacs_modules_path=""
emacs_snippets_path=""
emacs_elpa_path=""

# Ask for path
askPath () {
    read -e -p "$1($2): " path
}

# Check if input is empty
checkPathInput () {
    if [[ "$path" != '' ]]
    then
        set -- $path
    fi
    echo $1
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
    echo $emacs_modules_path
    echo $emacs_snippets_path
    echo $emacs_elpa_path
    confirm_value= ask "Is this correct?" Y
    if [[ $confirm_value == 1 ]]; then
        exit
    fi
}

# Check if file exists
checkFileExists () {
    if [ -f "$1" ]
    then
        return 0
    else
        return 1
    fi
}

# Check if directory exists
checkDirectoryExists () {
    if [ -d "$1" ]
    then
        return 0
    else
        return 1
    fi
}

backupEmacs () {
    echo "Backup .emacs..."
    mv $emacs_path "$emacs_path.bu"
}

backupEmacsModules () {
    echo "Backup emacs modules..."
    mv $emacs_modules_path "$emacs_modules_path.bu"
}

backupEmacsYaSnippets () {
    echo "Backup emacs YaSnippets..."
    mv $emacs_snippets_path "$emacs_snippets_path.bu"
}

linkEmacs () {
    cd ${emacs_path//.emacs/}
    echo "Linking emacs..."
    ln -s $current_emacs ".emacs"
}

linkEmacsModules () {
    cd $emacs_dir_path
    echo "Linking emacs modules..."
    ln -s $current_emacs_modules "modules"
}

linkEmacsYaSnippets () {
    cd $emacs_dir_path
    echo "Linking YaSnippets..."
    ln -s $current_emacs_snippets "snippets"
}

createElpa () {
    echo "Creating Elpa..."
    mkdir -p $emacs_elpa_path
}

userInput () {
    askPath "Enter path to .emacs" $emacs_path
    emacs_path=$(checkPathInput $emacs_path)
    emacs_path=${emacs_path/#~/$HOME}
    askPath "Enter path to user-emacs-directory" $emacs_dir_path
    emacs_dir_path=$(checkPathInput $emacs_dir_path)
    emacs_dir_path=${emacs_dir_path/#~/$HOME}
    emacs_modules_path="$emacs_dir_path/modules"
    emacs_modules_path=${emacs_modules_path//\/\//\/}
    emacs_snippets_path="$emacs_dir_path/snippets"
    emacs_snippets_path=${emacs_snippets_path//\/\//\/}
    emacs_elpa_path="$emacs_dir_path/elpa"
    emacs_elpa_path=${emacs_elpa_path//\/\//\/}
    checkIfCorrect
}

checkFiles () {
    if checkFileExists $emacs_path
    then
        backupEmacs
    fi
    linkEmacs

    if checkDirectoryExists $emacs_modules_path
    then
        backupEmacsModules
    fi
    linkEmacsModules

    if checkDirectoryExists $emacs_snippets_path
    then
        backupEmacsYaSnippets
    fi
    linkEmacsYaSnippets

    if checkDirectoryExists $emacs_elpa_path
    then
        echo "Elpa already exists. Skipping..."
    else
        createElpa
    fi
}

main () {
    userInput
    checkFiles
    echo "Linking done."
    exit 0
}

main
