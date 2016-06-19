;; info -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'glof)
(require 'seq)

(require 'hoarder-package "hoarder/package")

(cl-defun hoarder:option-info (package)
  (cl-letf ((ipath (glof:get package :info)))
    (hoarder:%option-info ipath package)))

(cl-defmethod hoarder:%option-info ((paths list) package)
  (seq-each
   (lambda (path)
     (hoarder:option-info-set-infopath
      (expand-file-name path (glof:get package :path))))
   paths))

(cl-defmethod hoarder:%option-info ((ipath string) package)
  (cl-letf ((path (expand-file-name ipath (glof:get  package :path))))
    (hoarder:message "add directory %s to INFOPATH" path)
    (hoarder:option-info-set-infopath
     path)))

(cl-defun hoarder:option-info-set-infopath (ipath)
  (setenv "INFOPATH"
          (seq-concatenate 'string ipath ":"
                           (getenv "INFOPATH"))))

(provide 'hoarder-option-info)

;;; info.el ends here
