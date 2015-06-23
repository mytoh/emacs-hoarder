;/bin/tcsh;; info -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'hoarder-package "hoarder/package")

(cl-defmethod hoarder:option-info ((package hoarder:<package>))
  (cl-letf ((info-path (hoarder:package-info package)))
    (hoarder:%option-info info-path package)))

(cl-defmethod hoarder:%option-info ((info-paths list) (package hoarder:<package>))
  (seq-each
   (lambda (path)
     (hoarder:option-info-set-infopath
      (expand-file-name path (hoarder:package-path package))))
   info-paths))

(cl-defmethod hoarder:%option-info ((info-path string) (package hoarder:<package>))
  (cl-letf ((path (expand-file-name info-path (hoarder:package-path package))))
    (hoarder:message "add directory %s to INFOPATH" path)
    (hoarder:option-info-set-infopath
     path)))

(cl-defun hoarder:option-info-set-infopath (path)
  (setenv "INFOPATH"
          (seq-concatenate 'string path ":"
                           (getenv "INFOPATH"))))

(provide 'hoarder-option-info)

;;; info.el ends here
