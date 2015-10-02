#!/bin/sh

lein run -m babel.espanol.writer/todos
lein run -m babel.francais.writer/tout
lein run -m babel.italiano.writer/tutti
lein run -m babel.english.writer/translate "it"
