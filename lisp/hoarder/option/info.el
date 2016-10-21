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

(cl-defun hoarder:%option-info (ipaths package)
  (cl-etypecase ipaths
    (list
     (seq-each
      (lambda (ipath)
        (hoarder:option-info-set-infopath
         (expand-file-name ipath (glof:get package :path))))
      ipaths))
    (string
     (cl-letf ((ipath (expand-file-name ipaths (glof:get  package :path))))
       (hoarder:message "add directory %s to INFOPATH" ipath)
       (hoarder:option-info-set-infopath
        ipath)))))

(cl-defun hoarder:option-info-set-infopath (ipath)
  (setenv "INFOPATH"
          (seq-concatenate 'string ipath ":"
                           (getenv "INFOPATH"))))

(provide 'hoarder-option-info)

;;; info.el ends here
