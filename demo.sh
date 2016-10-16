#!/bin/sh
set -euo pipefail
IFS=$'\n\t'

NUM=${1:-5}
DEMO=${2:-""}

#TODO: run other languages besides english.
lein run -m babel.english.demo/demo ${NUM} "${DEMO}"

#lein run -m babel.italiano.demo/demo ${NUM} \"${DEMO}\"


