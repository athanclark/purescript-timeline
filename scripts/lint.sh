#! /bin/bash

mkdir --parents tmp/

shopt -s globstar

FILES=src/**/*.purs
REMOVE=0

for file in $FILES
do
    mkdir --parents "$(dirname "tmp/$file")"

    if [ $# -eq 0 ]
    then
        purty $file > tmp/$file
        diff --brief $file tmp/$file
        if [ $? -ne 0 ]
        then
            REMOVE=1
        fi
    elif [ $1 == "write" ]
    then
        purty --write $file
    else
        echo "Bad argument. Only one allowed is write"
    fi
done

if [ $REMOVE -eq 0 ]
then
    rm -r tmp/
else
    exit 1
fi
