#!/usr/bin/env bash

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case $OSTYPE in
    linux*)
        export LD_LIBRARY_PATH="$WORK_DIR/lib/"
        export ALSOFT_DRIVERS="-jack,$ALSOFT_DRIVERS"
        ;;
    darwin*)
        export DYLD_LIBRARY_PATH="$WORK_DIR/lib/"
        ;;
esac

cd $WORK_DIR
./mortar-combat.bin mortar-combat.conf.lisp
