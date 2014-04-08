;;; run-test.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path default-directory)
(require 'vendle)

(ert-deftest vendle-test-vendle-path ()
  (should vendle-directory)
  (should (string-equal vendle-directory (file-name-as-directory (expand-file-name "~/.emacs.d/vendle")))))

(cl-defun main ()
  (ert-run-tests-batch-and-exit))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
