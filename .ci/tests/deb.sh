#!/usr/bin/env bash

set -xe

test -f *.deb

test $(dpkg-deb -f *.deb Depends | grep -c libsqlite3-0) = 1

# additional-dependencies
test $(dpkg-deb -f *.deb Depends | grep -c emacs) = 1

# make sure we don't have any cached libraries
rm -rf ~/.cache

dpkg -i *.deb || true

apt-get -f install -y

other-random-name
