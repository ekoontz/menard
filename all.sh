#!/bin/sh

lein run -m babel.espanol.writer/todos
lein run -m babel.english.writer/translate "es"

lein run -m babel.francais.writer/tout
lein run -m babel.english.writer/translate "fr"

# If you set CHECK_EXISTING="false" then generate no matter what
# (ignore existing expressions, if any.
# Otherwise, only generate if needed (if no existing expressions
# are in the database)

if [ "${CHECK_EXISTING}" != "false" ]; then
    lein run -m babel.italiano.writer/tutti
    lein run -m babel.english.writer/translate "it"
else
    lein run -m babel.italiano.writer/tutti "all" "all" "$(date)"
    lein run -m babel.english.writer/translate "it" "all" "$(date)"
fi


    
