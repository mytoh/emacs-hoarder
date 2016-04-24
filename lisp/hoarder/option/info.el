;; info -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'glof)
(require 'seq)

(require 'hoarder-package "hoarder/package")

(cl-defun hoarder:option-info (package)
  (cl-letf ((info-path (glof:get package :info)))
    (hoarder:%option-info info-path package)))

(cl-defmethod hoarder:%option-info ((info-paths list) package)
  (seq-each
   (lambda (path)
     (hoarder:option-info-set-infopath
      (expand-file-name path (glof:get package :path))))
   info-paths))

(cl-defmethod hoarder:%option-info ((info-path string) package)
  (cl-letf ((path (expand-file-name info-path (glof:get  package :path))))
    (hoarder:message "add directory %s to INFOPATH" path)
    (hoarder:option-info-set-infopath
     path)))

(cl-defun hoarder:option-info-set-infopath (path)
  (setenv "INFOPATH"
          (seq-concatenate 'string path ":"
                           (getenv "INFOPATH"))))

(provide 'hoarder-option-info)

;;; info.el ends here
