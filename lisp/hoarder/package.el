;;; package.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-source-hg  "hoarder/source/hg")
(require 'hoarder-source-local "hoarder/source/local")

;;;; package

(defvar hoarder:<package>
  '(:type nil
    :name ""
    :url ""
    :path ""
    :load-path ""
    :site ""
    :compile nil
    :dependencies nil
    :build nil
    :info nil
    :origin ""
    :tags nil
    :description ""
    :recursive nil
    :branch nil
    :download t
    :memo ""
    ;; :category 
    ;; :keyword
    ))

(cl-defun hoarder:make-package (source option)
  (cl-letf ((s (string-trim source))
            (type (glof:get option :type)))
    (pcase s
      ((pred hoarder:source-site-github-p)
       (hoarder:make-package-github
        (hoarder:source-site-format-github s) option))
      ((or (pred hoarder:source-git-p)
          (guard (cl-equalp type :git)))
       (hoarder:make-package-git
        s option))
      ((guard (cl-equalp type :hg))
       (hoarder:make-package-hg s option)))))

(cl-defun hoarder:delete-package-files (package)
  (when (and (not (cl-equalp :local (glof:get package :type)))
           (file-exists-p (glof:get package :path)))
    (hoarder:message "removing files")
    (delete-directory (glof:get package :path) 'recursive)))

(provide 'hoarder-package)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
