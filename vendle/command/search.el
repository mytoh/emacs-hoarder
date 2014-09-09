;;; search -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; search
(cl-defun vendle:search-registered (_key _term)
  (cl-remove-if-not
   (lambda (p)
     (pcase _term
       (`name
        (cl-equalp _key (vendle:package-name p)))
       (`type
        (cl-equalp _key (vendle:package-type p)))
       (`path
        (cl-equalp _key (vendle:package-path p)))
       (`load-path
        (cl-equalp _key (vendle:package-load-path p)))))
   *vendle-package-list*))

(cl-defun vendle:registered-p (name)
  (if (vendle:search-registered name 'name)
      t nil))

(provide 'vendle-search)

;;; search.el ends here
