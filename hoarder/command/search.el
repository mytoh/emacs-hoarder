;;; search -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'glof)
(require 'subr-x)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

;;;; search
(cl-defun hoarder:search-registered (key target)
  (hoarder::filter
   (lambda (p)
     (cl-equalp target (glof:get p key)))
   hoarder:*packages*))

(cl-defun hoarder:registered-p (name)
  (if (not (seq-empty-p
            (hoarder:search-registered :name name)))
      t nil))

(provide 'hoarder-search)

;;; search.el ends here
