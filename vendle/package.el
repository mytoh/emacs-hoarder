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
          :type (or list string)
          :initform nil
          :accessor vendle:package-build)
   (info :initarg :info
         :type (or list string)
         :initform nil
         :accessor vendle:package-info)
   (origin :initarg :origin
           :type string
           :initform ""
           :accessor vendle:package-origin)
   (tag :initarg :tag
        :type (or list string)
        :initform nil
        :accessor vendle:package-tag)
   (desc :initarg :desc
         :type string
         :initform ""
         :accessor vendle:package-desc)))

(cl-defun vendle:make-package (source option)
  (cl-letf ((s (string-trim source)))
    (cond ((vendle:source-site-github-p s)
           (vendle:make-package-github
            (vendle:source-site-format-github s) option))
          ((vendle:source-git-p s)
           (vendle:make-package-git
            s option)))))

(provide 'vendle-package)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
