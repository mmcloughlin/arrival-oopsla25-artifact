#!/usr/bin/env bash

set -euo pipefail

git -C arrival show fdf2e56cf56e5956f2a8b9a675ce9ef168af5414 >data/sdiv_fix.patch
