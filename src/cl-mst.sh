#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
gpp=`type -P g++`

if [ $1 = "init" ]
then
    
    > $DIR/.tmp
    if [ -z "$gpp" ]
    then
	echo '(warn "cl-mst:boruvka requires the command line G++")' > $DIR/.tmp
    fi
    exit 0

else

    rm -f $DIR/boruvka
    cat $DIR/boruvka.cpp $DIR/.tmp > $DIR/.tmp.cpp 
    $gpp -o $DIR/boruvka $DIR/.tmp.cpp 
    $DIR/boruvka > $DIR/.tmp

fi
