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
    START_DATE=$(date)

    # generate all-new Italian
    lein run -m babel.italiano.writer/tutti "all" "all" "${START_DATE}"

    # generate all-new translations of those Italian sentences.
    lein run -m babel.english.writer/translate "it" "all" "${START_DATE}"
fi


    
