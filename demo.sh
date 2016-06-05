#!/bin/sh

NUM=$1
if [[ ! ${NUM} ]]; then
    NUM=5
fi

DEMO=$2
if [[ ! ${DEMO} ]]; then
    DEMO=""
fi

#TODO: run other languages besides english.
lein run -m babel.english.demo/demo ${NUM} "${DEMO}"

#lein run -m babel.italiano.demo/demo ${NUM} \"${DEMO}\"


