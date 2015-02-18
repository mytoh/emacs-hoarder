;;; uninstall -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

(require 'vendle-clean "vendle/command/clean")

;;;; uninstall
(cl-defmethod vendle:uninstall-package ((package vendle:package))
  (when (and (not (cl-equalp 'local (vendle:package-type package)))
             (file-exists-p (vendle:package-path package)))
    (vendle:message "unregister package info")
    (cl-delete-if
     (lambda (p) (cl-equalp (vendle:package-name package)
                       (vendle:package-name p)))
     *vendle-package-list*)
    (vendle:message "removing files")
    (delete-directory (vendle:package-path package) 'recursive)))

(cl-defun vendle:uninstall-package-by-name (name)
  (cl-letf ((target (vendle:search-registered "web-mode" 'name)))
    (vendle:uninstall-package (cl-first target))))

(provide 'vendle-uninstall)

;;; uninstall.el ends here
