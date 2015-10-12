;;; clean -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'glof)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

;;;; clean
(cl-defun hoarder:clean-packages ()
  (if-let ((paths (seq-remove
                   (lambda (dir)
                     (hoarder:search-registered :path dir))
                   (directory-files hoarder-directory 'absolute (rx (not (any ".")))))))
      (seq-each
       (lambda (p)
         (hoarder:message "clean %s" p)
         (delete-directory p t))
       paths)))

;;;###autoload
(cl-defun hoarder:clean ()
  (interactive)
  (hoarder:message "package clean start")
  (hoarder:clean-packages)
  (hoarder:message "package clean finish"))

(provide 'hoarder-clean)

;;; clean.el ends here
