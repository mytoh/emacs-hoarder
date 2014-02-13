;;; package.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))

(require 'vendle-site-github "vendle/site/github")
(require 'vendle-source-git "vendle/source/git")

;;;; package

(cl-defstruct vendle:package
  type name url path site compile)

(cl-defun vendle:make-package (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-github
          (vendle:source-site-format-github source) info))))

(cl-defun vendle:make-package-github (source info)
  (make-vendle:package :type 'git
                       :site "github"
                       :name (vendle:make-package-name source info)
                       :path (vendle:make-package-path source info)
                       :url (cl-concatenate 'string "git://github.com/" source)
                       :compile (if info
                                    (cl-getf info :compile)
                                  t)))

(cl-defun vendle:make-package-local (source info)
  (make-vendle:package :type 'local
                       :name (vendle:make-package-name-local source info)
                       :path source
                       :url nil
                       :compile nil))

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

(cl-defun vendle:make-package-path (source info)
  (cond ((vendle:source-site-github-p source)
         (vendle:make-package-path-github (vendle:source-site-format-github source) info))))

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
          nil))
    nil))


(provide 'vendle-package)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
