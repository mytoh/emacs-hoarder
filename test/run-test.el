;;; run-test.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path default-directory)
(require 'vendle)

(ert-deftest vendle-test-initialize ()
  (vendle:initialize)
  (should (string-equal vendle-directory (file-name-as-directory (expand-file-name "~/.emacs.d/vendle"))))
  (should (file-exists-p vendle-directory))
  (should (file-directory-p vendle-directory))
  (when (file-exists-p vendle-directory)
    (delete-directory vendle-directory 'recursive)))

(cl-defun main ()
  (ert-run-tests-batch-and-exit))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
