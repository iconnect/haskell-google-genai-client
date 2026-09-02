#!/usr/bin/env sh
# Fetches the newest Gemini API Discovery document into spec/.
# After running: cabal run -f codegen genai-codegen -- --spec spec/generativelanguage-v1beta.json --lib lib --tests tests
set -eu
url='https://generativelanguage.googleapis.com/$discovery/rest?version=v1beta'
out="$(cd "$(dirname "$0")/.." && pwd)/spec/generativelanguage-v1beta.json"
mkdir -p "$(dirname "$out")"
curl -fsSL "$url" -o "$out"
printf 'revision: %s\n' "$(jq -r .revision "$out")"
