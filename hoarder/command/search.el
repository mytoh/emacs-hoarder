;;; search -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
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
        (cl-equalp key (hoarder:package-name p)))
       (`type
        (cl-equalp key (hoarder:package-type p)))
       (`path
        (cl-equalp key (hoarder:package-path p)))
       (`load-path
        (cl-equalp key (hoarder:package-load-path p)))))
   hoarder:*packages*))

(cl-defun hoarder:registered-p (name)
  (if (hoarder:search-registered name 'name)
      t nil))

(provide 'hoarder-search)

;;; search.el ends here
