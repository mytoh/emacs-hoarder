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

(ert-deftest vendle-test-initialize-with-custom-directory ()
  (cl-letf ((vendle-test-directory "~/vendle-testing"))
    (vendle:initialize vendle-test-directory)
    (should (string-equal vendle-directory vendle-test-directory))
    (should (file-exists-p vendle-test-directory))
    (should (file-directory-p vendle-test-directory))
    (when (file-exists-p vendle-test-directory)
      (delete-directory vendle-test-directory 'recursive))))

(ert-deftest vendle-test-register ()
  (vendle:initialize)
  (vendle:register "mytoh/fish-mode")
  (should (vendle:search-registered "fish-mode" 'name))
  (when (file-exists-p vendle-test-directory)
    (delete-directory vendle-test-directory 'recursive)))

(ert-deftest vendle-test-register-theme ()
  (vendle:initialize)
  (vendle:register-theme "sabof/hyperplane-theme")
  (should (vendle:search-registered "hyperplane-theme" 'name))
  (when (file-exists-p vendle-test-directory)
    (delete-directory vendle-test-directory 'recursive)))

(cl-defun main ()
  (ert-run-tests-batch-and-exit))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
