;;; check -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; check
(cl-defun vendle:check-packages ()
  (vendle:foreach-package-list 'vendle:install-package))

;;;; commands

;;;###autoload
(cl-defun vendle-check ()
  "Install packages using `vendle:install-packages'"
  (interactive)
  (vendle:message "package check start")
  (vendle:check-packages)
  (vendle:message "package check finish"))


(provide 'vendle-check)

;;; check.el ends here
