;;; package.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-source-local "hoarder/source/local")

;;;; package

;; (defclass hoarder:<package> ()
;;   ((type :initarg :type
;;          :type symbol
;;          :initform nil
;;          :accessor hoarder:package-type)
;;    (name :initarg :name
;;          :type string
;;          :initform ""
;;          :accessor hoarder:package-name)
;;    (url :initarg :url
;;         :type string
;;         :initform ""
;;         :accessor hoarder:package-url)
;;    (path :initarg :path
;;          :type string
;;          :initform ""
;;          :accessor hoarder:package-path)
;;    (load-path :initarg :load-path
;;               :type (or list string)
;;               :initform ""
;;               :accessor hoarder:package-load-path)
;;    (site :initarg :site
;;          :type string
;;          :initform ""
;;          :accessor hoarder:package-site)
;;    (compile :initarg :compile
;;             :type symbol
;;             :initform nil
;;             :accessor hoarder:package-compile)
;;    (dependency :initarg :dependency
;;                :type list
;;                :initform nil
;;                :accessor hoarder:package-dependency)
;;    (build :initarg :build
;;           :type (or list string)
;;           :initform nil
;;           :accessor hoarder:package-build)
;;    (info :initarg :info
;;          :type (or list string)
;;          :initform nil
;;          :accessor hoarder:package-info)
;;    (origin :initarg :origin
;;            :type string
;;            :initform ""
;;            :accessor hoarder:package-origin)
;;    (tag :initarg :tag
;;         :type (or list string)
;;         :initform nil
;;         :accessor hoarder:package-tag)
;;    (desc :initarg :desc
;;          :type string
;;          :initform ""
;;          :accessor hoarder:package-desc)
;;    (recursive :initarg :recursive
;;               :type (or null t)
;;               :initform nil
;;               :accessor hoarder:package-recursive)
;;    (branch :initarg :branch
;;            :type (or null string)
;;            :initform nil
;;            :accessor hoarder:package-branch)))

(cl-defun hoarder:make-package (source option)
  (cl-letf ((s (string-trim source)))
    (pcase s
      ((pred hoarder:source-site-github-p)
       (hoarder:make-package-github
        (hoarder:source-site-format-github s) option))
      ((pred hoarder:source-git-p)
       (hoarder:make-package-git
        s option)))))

(provide 'hoarder-package)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
