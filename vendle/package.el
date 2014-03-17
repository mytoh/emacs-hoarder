;;; package.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))

(require 'vendle-site-github "vendle/site/github")
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
              :type string
              :initform ""
              :accessor vendle:package-load-path)
   (site :initarg :site
         :type string
         :initform ""
         :accessor vendle:package-site)
   (compile :initarg :compile
            :type symbol
            :initform nil
            :accessor vendle:package-compile)))

(cl-defun vendle:make-package (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-github
          (vendle:source-site-format-github source) info))))

(cl-defun vendle:make-package-github (source info)
  (cl-letf ((name (vendle:make-package-name source info))
            (path (vendle:make-package-path source info))
            (load-path (vendle:make-package-load-path source info)))
    (vendle:package name
                    :type 'git
                    :site "github"
                    :name name
                    :path path
                    :load-path load-path
                    :url (cl-concatenate 'string "git://github.com/" source)
                    :compile (if info
                                 (cl-getf info :compile)
                               t))))

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

(cl-defun vendle:make-package-name (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-name-github
          (vendle:source-site-format-github source) info))))

(cl-defun vendle:make-package-name-github (source info)
  (if info
      (cl-letf ((name (cl-getf info :name)))
        (if name
            name
          (cadr (split-string source "/"))))
    (cadr (split-string source "/"))))

(cl-defun vendle:make-package-name-local (source info)
  (if info
      (cl-letf ((name (cl-getf info :name)))
        (if name
            name
          (file-name-nondirectory source)))
    (file-name-nondirectory source)))

(cl-defun vendle:make-package-load-path (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-load-path-github (vendle:source-site-format-github source) info))))

(cl-defun vendle:make-package-load-path-github (source info)
  (cl-letf ((path (if info
                      (cl-letf ((path (cl-getf info :load-path))
                                (name (vendle:make-package-name source info)))
                        (if path
                            (cl-concatenate 'string
                                            name  "/"  path)
                          name))
                    (vendle:make-package-name source info))))
    (expand-file-name path vendle-directory)))

(cl-defun vendle:make-package-load-path-local (source info)
  (cl-letf ((local-path (if info
                            (cl-letf ((path (cl-getf info :load-path)))
                              (if path
                                  (expand-file-name path source)
                                source))
                          source)))
    local-path))

(cl-defun vendle:make-package-path (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-load-path-github (vendle:source-site-format-github source) info))))

(cl-defun vendle:make-package-path-github (source info)
  (cl-letf ((path (if info
                      (cl-letf ((path (cl-getf info :path))
                                (name (vendle:make-package-name source info)))
                        (if path
                            (cl-concatenate 'string
                                            name  "/"  path)
                          name))
                    (vendle:make-package-name source info))))
    (expand-file-name path vendle-directory)))


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
