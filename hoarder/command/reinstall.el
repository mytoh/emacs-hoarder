;;; reinstall -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

(cl-defun hoarder:reinstall-package (package)
  (hoarder:uninstall-package package)
  (hoarder:install-package package))

;;;###autoload
(cl-defun hoarder:reinstall ()
  (interactive)
  (hoarder:message "package reinstall start")
  (hoarder:reinstall-package)
  (hoarder:message "package reinstall finish"))

(provide 'hoarder-reinstall)

;;; reinstall.el ends here
