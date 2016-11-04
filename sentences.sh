set -euo pipefail
IFS=$'\n\t'

lein run -m babel.english/sentences 100
