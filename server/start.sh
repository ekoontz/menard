#!/bin/bash
# Start the language endpoint server in the background. This will generate
# expressions for the user and parse their responses. It listens on port
# 3000, where it handles requests to:
# - generate expressions for the user
# - parse the user's guesses of answers to those expressions
# - provides various linguistic resources (lexicons,
#   grammars and morphological rules).
#
# The LANGUAGE_ENDPOINT_URL is the URL for the UI server
# It must be provided as an environmental variable to this
# script. The language endpoint needs that URL so it can set the
# header: 'Access-Control-Allow-Origin' to that.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
MENARD_DIR=${SCRIPT_DIR}/..
cd ${SCRIPT_DIR}


echo "** start.sh ** starting the language server from directory: ${SCRIPT_DIR} using MENARD_DIR: ${MENARD_DIR} and with hostname: ${HOSTNAME}"

MENARD_DIR=${MENARD_DIR} ORIGIN=http://${HOSTNAME}:3449 lein ring server-headless
