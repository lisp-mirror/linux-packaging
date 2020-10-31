#!/usr/bin/env bash

set -xe

test -f *.pkg.tar.xz

pacman --noconfirm -U *.pkg.tar.xz

test $(pacman -Qi random-name | grep -c sqlite) = 1

# additional-dependencies
test $(pacman -Qi random-name | grep -c emacs) = 1

# TODO: installation like in deb.sh
