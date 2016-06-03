#!/bin/sh

NUM=$1

if [[ ! $NUM ]]; then
    NUM=5
fi

#TODO: run other languages besides english.
lein run -m babel.english.demo/demo $NUM

lein run -m babel.italiano.demo/demo $NUM

