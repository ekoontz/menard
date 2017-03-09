#!/bin/sh

lein run -m babel.espanol.writer/todos
lein run -m babel.english.writer/translate "es"

lein run -m babel.francais.writer/tout
lein run -m babel.english.writer/translate "fr"

# 1. only generate if needed (if no existing expressions are in the database)
lein run -m babel.italiano.writer/tutti
lein run -m babel.english.writer/translate "it"

# 2. generate no matter what (ignore existing expressions, if any)
#lein run -m babel.italiano.writer/tutti "all" "all" "$(date)"
#lein run -m babel.english.writer/translate "it" "all" "$(date)"
