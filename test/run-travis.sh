#!/usr/bin/env sh

EMACS=${EMACS:-$(which emacs)}

git clone git://github.com/mytoh/vendle /home/travis/vendle

${EMACS} --version
${EMACS} -Q --script test/run-test.el
