#!/bin/sh
set -euo pipefail
IFS=$'\n\t'

LANGUAGE=${1:"it"}

lein run -m babel.directory/generate-all "${LANGUAGE}"


