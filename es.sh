#!/bin/sh

lein run -m babel.espanol.writer/todos
lein run -m babel.english.writer/translate "es"


