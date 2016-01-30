;;; uninstall -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'glof)
(require 'colle)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

(require 'hoarder-clean "hoarder/command/clean")

;;;; uninstall
(cl-defun hoarder:uninstall-package (package)
  (when (and (not (cl-equalp :local (glof:get package :type)))
             (file-exists-p (glof:get package :path)))
    (hoarder:message "unregister package info")
    (setq hoarder:*packages*
          (colle:remove
           (lambda (p) (hoarder:package-compare-fn p package))
           hoarder:*packages*))
    (hoarder:message "removing files")
    (delete-directory (glof:get package :path) 'recursive)))

(cl-defun hoarder:uninstall-package-by-name (name)
  (cl-letf ((target (hoarder:search-registered :name name)))
    (hoarder:uninstall-package (colle:first target))))

(provide 'hoarder-uninstall)

;;; uninstall.el ends here
