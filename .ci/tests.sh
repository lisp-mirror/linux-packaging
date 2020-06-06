#!/usr/bin/env bash

set -xe

t=$1

test -f other-random-name

./.ci/tests/"$t".sh
