#!/usr/bin/env bash

set -euo pipefail

archive_file="arrival.zip"

git-archive-all "${archive_file}"
