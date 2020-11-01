#!/usr/bin/env bash

set -xe

test -f *.rpm

test $(rpm -qpR *.rpm | grep -c sqlite-libs) = 1

# additional-dependencies
test $(rpm -qpR *.rpm | grep -c emacs) = 1

# TODO: installation like in deb.sh
