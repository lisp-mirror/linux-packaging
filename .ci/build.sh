#!/usr/bin/env bash

set -xe

t=$1

gem install --no-document fpm &> /dev/null

export PATH=~/.gem/ruby/$(ls ~/.gem/ruby)/bin:$PATH

git clone --depth=1 --branch=sbcl-2.0.5 https://github.com/sbcl/sbcl.git ~/sbcl &> /dev/null
(
    cd ~/sbcl
    set +e
    sh make.sh --fancy --with-sb-linkable-runtime --with-sb-dynamic-core &> sbcl-build.log
    code=$?
    set -e
    test $code = 0 || (cat sbcl-build.log && exit 1)

    sh install.sh &> /dev/null
)

export SBCL_HOME=/usr/local/lib/sbcl

curl -O https://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit &> /dev/null
rm quicklisp.lisp

mkdir -p ~/common-lisp
git clone --depth=1 https://github.com/privet-kitty/wild-package-inferred-system.git ~/common-lisp/wild-package-inferred-system/ &> /dev/null
git clone --depth=1 https://github.com/cffi/cffi.git ~/common-lisp/cffi/ &> /dev/null
git clone --depth=1 https://gitlab.common-lisp.net/asdf/asdf.git ~/common-lisp/asdf/ &> /dev/null

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$PWD/\")" > ~/.config/common-lisp/source-registry.conf.d/asdf.conf

sbcl \
    --eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (sb-ext:quit :unix-status -1)))' \
    --load ~/quicklisp/setup.lisp \
    --eval "(ql:quickload :linux-packaging)" \
    --eval "(ql:quickload :linux-packaging-tests/$t)" \
    --eval "(asdf:make :linux-packaging-tests/$t)" \
    --quit
