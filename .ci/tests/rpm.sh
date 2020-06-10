#!/usr/bin/env bash

set -xe

test -f *.rpm

test $(rpm -qpR *.rpm | grep -c sqlite-libs) = 1