#!/usr/bin/env sh

SITELISP_DIR="site-lisp"
EMACSCONF_DIR="emacs-conf"

if [ -z "$2" ]; then
    BASEPATH=~
else
    BASEPATH=$2
fi

if [ -z "$1" ]; then
    echo
    echo "  setup.sh conf [PATH] - copy emacs-conf files to PATH/emacs-conf"
    echo "  setup.sh lisp [PATH] - download used packages to PATH/site-lisp"
    echo "  setup.sh lisp-cleanup [PATH] - delete generated backups"
    echo
    echo "  if [PATH] is ommitted, user home dir (~/) is used"
fi

if [ "$1" = "conf" ]; then
    if [ ! -d "$BASEPATH/$EMACSCONF_DIR" ]; then
        mkdir -p "$BASEPATH/$EMACSCONF_DIR"
    fi

    rsync -av *.el "$BASEPATH/$EMACSCONF_DIR/"
fi

if [ "$1" = "lisp" ]; then
    if [ ! -d "$BASEPATH/$SITELISP_DIR" ]; then
        mkdir -p "$BASEPATH/$SITELISP_DIR"
    fi

    cd "$BASEPATH/$SITELISP_DIR"

    mv csharp-mode.el csharp-mode.el.backup
    wget https://raw.githubusercontent.com/josteink/csharp-mode/master/csharp-mode.el
    mv shader-mode.el shader-mode.el.backup
    wget https://raw.githubusercontent.com/midnightSuyama/shader-mode/master/shader-mode.el
    mv visual-fill-column.el visual-fill-column.el.backup
    wget https://raw.githubusercontent.com/joostkremers/visual-fill-column/master/visual-fill-column.el
fi

if [ "$1" = "lisp-cleanup" ]; then
    cd "$BASEPATH/$SITELISP_DIR"
    rm -fv *.el.backup
fi
