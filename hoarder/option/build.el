;/bin/tcsh;; build -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)

(require 'seq)
(require 'hoarder-package "hoarder/package")

(cl-defmethod hoarder:option-build ((package hoarder:<package>))
  (when (hoarder:package-build package)
    (cl-letf ((commands (hoarder:package-build package))
              (path (file-name-as-directory (hoarder:package-path package))))
      (seq-each
       (lambda (c)
         (cl-letf ((default-directory path))
           (shell-command c nil nil)))
       commands))))

(provide 'hoarder-option-build)

;;; build.el ends here
