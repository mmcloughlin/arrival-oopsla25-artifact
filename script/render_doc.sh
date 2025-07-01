#!/usr/bin/env bash

set -euo pipefail

output_file="$1"

pandoc \
    --from=gfm \
    --to=pdf \
    -V colorlinks=true \
    -o "${output_file}" \
    README.md
