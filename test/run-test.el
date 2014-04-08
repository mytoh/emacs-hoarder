;;; run-test.el -*- lexical-binding: t -*-

(add-to-list 'load-path (file-name-directory (directory-file-name default-directory)))
(require 'ert)
(require 'cl-lib)
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
