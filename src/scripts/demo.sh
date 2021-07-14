#!/bin/sh

git log -1
git status
echo "(load \"menard/translate\")(menard.translate/demo)" | lein repl
