;;; check -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

;;;; check
(cl-defun hoarder:check-packages ()
  (hoarder:foreach-package-list 'hoarder:install-package))

;;;; commands

;;;###autoload
(cl-defun hoarder:check ()
  "Install packages using `hoarder:install-packages'"
  (interactive)
  (hoarder:message "package check start")
  (hoarder:check-packages)
  (hoarder:message "package check finish"))


(provide 'hoarder-check)

;;; check.el ends here
