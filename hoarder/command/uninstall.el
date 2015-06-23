;;; uninstall -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

(require 'hoarder-clean "hoarder/command/clean")

;;;; uninstall
(cl-defmethod hoarder:uninstall-package ((package hoarder:<package>))
  (when (and (not (cl-equalp 'local (hoarder:package-type package)))
             (file-exists-p (hoarder:package-path package)))
    (hoarder:message "unregister package info")
    (cl-delete-if
     (lambda (p) (cl-equalp (hoarder:package-name package)
                       (hoarder:package-name p)))
     hoarder:*packages*)
    (hoarder:message "removing files")
    (delete-directory (hoarder:package-path package) 'recursive)))

(cl-defun hoarder:uninstall-package-by-name (name)
  (cl-letf ((target (hoarder:search-registered "web-mode" 'name)))
    (hoarder:uninstall-package (cl-first target))))

(provide 'hoarder-uninstall)

;;; uninstall.el ends here
