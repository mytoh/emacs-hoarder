;;; package.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-source-local "hoarder/source/local")

;;;; package

(defvar hoarder:package-template
  '(:type nil
    :name ""
    :url ""
    :path ""
    :load-path ""
    :site ""
    :compile nil
    :dependency nil
    :build nil
    :info nil
    :origin ""
    :tag nil
    :desc ""
    :recursive nil
    :branch nil))

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