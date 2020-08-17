#!/bin/bash

if [ -z "$CUBIX_DOC_EXPORT" ]
then
    echo "Must set CUBIX_DOC_EXPORT to the location to export. Typically, this will be a subdirectory of cubix-www"
    exit 1
fi

stack clean
stack haddock --haddock-arguments "-o $CUBIX_DOC_EXPORT"
