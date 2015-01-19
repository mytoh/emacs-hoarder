;;; reinstall -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

(cl-defmethod vendle:reinstall-package ((package vendle:package))
  (vendle:uninstall-package package)
  (vendle:install-package package))

;;;###autoload
(cl-defun vendle-reinstall ()
  (interactive)
  (vendle:message "package reinstall start")
  (vendle:reinstall-package)
  (vendle:message "package reinstall finish"))

(provide 'vendle-reinstall)

;;; reinstall.el ends here
