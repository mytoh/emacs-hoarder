;;; hg -- hg -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'pcase)

(require 'glof)
(require 'colle)

(cl-defun hoarder:make-package-hg (source option)
  (cl-letf ((name (hoarder:make-package-name-hg source option))
            (path (hoarder:make-package-path-hg source option))
            (lpath (hoarder:make-package-load-path-hg source option))
            (origin (hoarder:make-package-origin-git source option))
            (comp (hoarder:make-package-compile-hg source option)))
    (glof:plist
     :branch (glof:get option :branch)
     :build (glof:get option :build nil)
     :compile comp
     :dependency (glof:get option :dependency nil)
     :desc (glof:get option :desc "")
     :download (glof:get option :download t)
     :info (glof:get option :info nil)
     :load-path lpath
     :name name
     :origin origin
     :path path
     :recursive (glof:get option :recursive)
     :site ""
     :tag (glof:get option :tag nil)
     :type :hg
     :url source)))

(cl-defun hoarder:make-package-name-hg (source option)
  (pcase option
    (`nil (file-name-base source))
    (_
     (glof:get option :name
               (file-name-base source)))))

(cl-defun hoarder:make-package-path-hg (source option)
  (cl-letf ((path (glof:get option :path))
            (origin (hoarder:trim-protocol source)))
    (if path
        (expand-file-name path hoarder-directory)
      (expand-file-name origin hoarder-directory))))

(cl-defun hoarder:make-package-load-path-hg (source option)
  (cl-letf ((path (glof:get option :load-path))
            (origin (hoarder:trim-protocol source)))
    (pcase path
      ((pred seq-empty-p)
       (hoarder:concat-path hoarder-directory origin))
      ((pred stringp)
       (hoarder:concat-path hoarder-directory origin path))
      (_
       (colle:map
        (lambda (p) (hoarder:concat-path hoarder-directory origin p))
        path)))))

(cl-defun hoarder:make-package-compile-hg (_source option)
  (if (glof:get option :build nil)
      nil
    (glof:get option :compile t)))

(provide 'hoarder-source-hg)

;;; hg.el ends here
