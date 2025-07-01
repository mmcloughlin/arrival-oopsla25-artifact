#!/usr/bin/env bash

set -exuo pipefail

base="arrival-oopsla25-artifact"
archive_file="${base}.zip"
doc_file="${base}.pdf"

# Setup output directory.
output_dir="build"
rm -rf "${output_dir}"
mkdir -p "${output_dir}"

# Render the documentation.
./script/render_doc.sh "${doc_file}"

# Prepare the archive.
git-archive-all --include="${doc_file}" "${archive_file}"

# Move to output directory.
mv "${doc_file}" "${archive_file}" "${output_dir}"
