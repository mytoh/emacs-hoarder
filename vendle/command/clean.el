;;; clean -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; clean
(cl-defun vendle:clean-packages ()
  (cl-letf ((paths (cl-remove-if
                    (lambda (dir)
                      (vendle:search-registered dir 'path))
                    (directory-files vendle-directory 'absolute (rx (not (any ".")))))))
    (if paths
        (cl-mapc
         (lambda (p)
           (vendle:message "clean %s" p)
           (delete-directory p t))
         paths))))

;;;###autoload
(cl-defun vendle-clean ()
  (interactive)
  (vendle:message "package clean start")
  (vendle:clean-packages)
  (vendle:message "package clean finish"))

(provide 'vendle-clean)

;;; clean.el ends here