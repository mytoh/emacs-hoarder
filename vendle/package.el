;;; package.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")

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
         :accessor vendle:package-deps)))

(cl-defun vendle:make-package (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-github
          (vendle:source-site-format-github source) info))
        ((vendle:source-git-p source)
         (vendle:make-package-git
          source info))))

(cl-defun vendle:make-package-local (source info)
  (cl-letf ((name (vendle:make-package-name-local source info))
            (load-path (vendle:make-package-load-path-local source info)))
    (vendle:package name
                    :type 'local
                    :name name
                    :path source
                    :load-path load-path
                    :url ""
                    :compile nil)))

(cl-defun vendle:make-package-name-local (source info)
  (if info
      (cl-letf ((name (cl-getf info :name)))
        (if name
            name
          (file-name-nondirectory source)))
    (file-name-nondirectory source)))

(cl-defun vendle:make-package-load-path-local (source info)
  (cl-letf ((local-path (if info
                            (cl-letf ((path (cl-getf info :load-path)))
                              (if path
                                  (expand-file-name path source)
                                source))
                          source)))
    local-path))

(cl-defun vendle:make-package-url-local (source info)
  (if info
      (cl-letf ((url (cl-getf info :url)))
        (if url
            url
          ""))
    ""))

(provide 'vendle-package)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
