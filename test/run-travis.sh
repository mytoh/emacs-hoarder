#!/usr/bin/env sh

EMACS=${EMACS:-$(which emacs)}

${EMACS} --version
${EMACS} -Q --batch --load test/run-test.el --funcall main
