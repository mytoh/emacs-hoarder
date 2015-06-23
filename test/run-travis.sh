#!/usr/bin/env sh

EMACS=${EMACS:-$(which emacs)}
SCRIPT="test/hoarder-test.el"
FUNCTION="main"

${EMACS} --version
${EMACS} -Q --batch --load ${SCRIPT} --funcall ${FUNCTION}
