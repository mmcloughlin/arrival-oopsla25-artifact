#!/usr/bin/env bash

set -exuo pipefail

base="arrival-oopsla25-artifact"
archive_file="${base}.zip"
doc_file="${base}.pdf"

# Render the documentation.
./script/render_doc.sh "${doc_file}"

# Prepare the archive.
git-archive-all --include="${doc_file}" "${archive_file}"
