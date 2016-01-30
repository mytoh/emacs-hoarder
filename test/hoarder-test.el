;;; hoarder-test.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'colle)

(add-to-list 'load-path default-directory)
(require 'hoarder)

(ert-deftest hoarder-test-initialize ()
  (hoarder:initialize)
  (should (string-equal hoarder-directory (file-name-as-directory (expand-file-name "~/.emacs.d/hoarder"))))
  (should (file-exists-p hoarder-directory))
  (should (file-directory-p hoarder-directory))
  (when (file-exists-p hoarder-directory)
    (delete-directory hoarder-directory 'recursive)))

(ert-deftest hoarder-test-initialize-with-custom-directory ()
  (cl-letf ((hoarder-test-directory "~/hoarder-testing"))
    (hoarder:initialize hoarder-test-directory)
    (should (string-equal hoarder-directory hoarder-test-directory))
    (should (file-exists-p hoarder-test-directory))
    (should (file-directory-p hoarder-test-directory))
    (when (file-exists-p hoarder-test-directory)
      (delete-directory hoarder-test-directory 'recursive))))

(ert-deftest hoarder-test-register ()
  (hoarder:initialize)
  (hoarder:register "mytoh/fish-mode")
  (should (hoarder:registered-p "fish-mode"))
  (when (file-exists-p hoarder-directory)
    (delete-directory hoarder-directory 'recursive)))

(ert-deftest hoarder-test-register-github ()
  (hoarder:initialize)
  (hoarder:register "mytoh/fish-mode")
  (should (hoarder:registered-p "fish-mode"))
  (should (cl-equalp :git
                     (glof:get (colle:first (hoarder:search-registered :name "fish-mode" ))
                               :type)))
  (should (cl-equalp "github"
                     (glof:get 
                      (colle:first (hoarder:search-registered :name "fish-mode" )) :site)))
  (when (file-exists-p hoarder-directory)
    (delete-directory hoarder-directory 'recursive)))

(ert-deftest hoarder-test-register-theme ()
  (hoarder:initialize)
  (hoarder:register-theme "sabof/hyperplane-theme")
  (should (hoarder:registered-p "hyperplane-theme"))
  (when (file-exists-p hoarder-directory)
    (delete-directory hoarder-directory 'recursive)))

(cl-defun main ()
  (ert-run-tests-batch-and-exit))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
