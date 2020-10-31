#!/usr/bin/env bash

set -xe

test -f *.deb

test $(dpkg-deb -f *.deb Depends | grep -c libsqlite3-0) = 1

# additional-dependencies
test $(dpkg-deb -f *.deb Depends | grep -c emacs) = 1
