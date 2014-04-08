;;; run-test.el -*- lexical-binding: t -*-

(add-to-list 'load-path (file-name-directory (directory-file-name default-directory)))
(require 'ert)
(require 'cl-lib)
(require 'vendle)

(ert-deftest vendle-test-vendle-path ()
  (should (string-equal vendle-directory "~/.emacs.d/vendle")))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
