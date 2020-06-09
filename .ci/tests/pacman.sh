#!/usr/bin/env bash

set -xe

test -f *.pkg.tar.xz

pacman --noconfirm -U *.pkg.tar.xz

test $(pacman -Qi random-name | grep -c sqlite) = 1
