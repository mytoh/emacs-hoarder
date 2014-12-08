;;; search -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio)
  (require 'subr-x))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; search
(cl-defun vendle:search-registered (key term)
  (cl-remove-if-not
   (lambda (p)
     (pcase term
       (`name
        (cl-equalp key (vendle:package-name p)))
       (`type
        (cl-equalp key (vendle:package-type p)))
       (`path
        (cl-equalp key (vendle:package-path p)))
       (`load-path
        (cl-equalp key (vendle:package-load-path p)))))
   *vendle-package-list*))

(cl-defun vendle:registered-p (name)
  (if (vendle:search-registered name 'name)
      t nil))

(provide 'vendle-search)

;;; search.el ends here
