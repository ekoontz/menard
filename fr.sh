#!/bin/sh
lein run -m babel.francais.writer/tout
lein run -m babel.english.writer/translate "fr"



