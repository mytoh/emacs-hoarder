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
(cl-defun hoarder:search-registered (key term)
  (seq-filter
   (lambda (p)
     (pcase term
       (`name
        (cl-equalp key (glof:get p :name)))
       (`type
        (cl-equalp key (glof:get p :type)))
       (`path
        (cl-equalp key (glof:get p :path)))
       (`load-path
        (cl-equalp key (glof:get p :load-path)))))
   hoarder:*packages*))

(cl-defun hoarder:registered-p (name)
  (if (hoarder:search-registered name 'name)
      t nil))

(provide 'hoarder-search)

;;; search.el ends here
