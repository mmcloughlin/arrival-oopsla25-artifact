#!/usr/bin/env bash

set -exuo pipefail

docker run \
    --interactive \
    --tty \
    --volume ".:/root/artifact" \
    --env "EVAL_DATA_DIR=/root/artifact/data/eval" \
    arrival
