#!/bin/sh
lein run -m babel.italiano.writer/tutti
lein run -m babel.english.writer/translate "it"



