;;; package.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-source-local "vendle/source/local")

;;;; package

(defclass vendle:package ()
  ((type :initarg :type
         :type symbol
         :initform nil
         :accessor vendle:package-type)
   (name :initarg :name
         :type string
         :initform ""
         :accessor vendle:package-name)
   (url :initarg :url
        :type string
        :initform ""
        :accessor vendle:package-url)
   (path :initarg :path
         :type string
         :initform ""
         :accessor vendle:package-path)
   (load-path :initarg :load-path
              :type (or list string)
              :initform ""
              :accessor vendle:package-load-path)
   (site :initarg :site
         :type string
         :initform ""
         :accessor vendle:package-site)
   (compile :initarg :compile
            :type symbol
            :initform nil
            :accessor vendle:package-compile)
   (deps :initarg :deps
         :type list
         :initform nil
         :accessor vendle:package-deps)
   (build :initarg :build
          :type list
          :initform nil
          :accessor vendle:package-build)
   (info :initarg :info
         :type list
         :initform nil
         :accessor vendle:package-info)))

(cl-defun vendle:make-package (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-github
          (vendle:source-site-format-github source) info))
        ((vendle:source-git-p source)
         (vendle:make-package-git
          source info))))

(provide 'vendle-package)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
