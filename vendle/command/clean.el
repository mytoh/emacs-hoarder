;;; clean -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; clean
(cl-defun vendle:clean-packages ()
  (if-let ((paths (cl-remove-if
                   (lambda (dir)
                     (vendle:search-registered dir 'path))
                   (directory-files vendle-directory 'absolute (rx (not (any ".")))))))
      (seq-each
       (lambda (p)
         (vendle:message "clean %s" p)
         (delete-directory p t))
       paths)))

;;;###autoload
(cl-defun vendle-clean ()
  (interactive)
  (vendle:message "package clean start")
  (vendle:clean-packages)
  (vendle:message "package clean finish"))

(provide 'vendle-clean)

;;; clean.el ends here
