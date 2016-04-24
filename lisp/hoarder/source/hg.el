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
    (glof:plist :type :hg
                :site ""
                :name name
                :path path
                :load-path lpath
                :url source
                :compile comp
                :dependency (glof:get option :dependency nil)
                :build (glof:get option :build nil)
                :info (glof:get option :info nil)
                :origin origin
                :tag (glof:get option :tag nil)
                :desc (glof:get option :desc "")
                :recursive (glof:get option :recursive)
                :branch (glof:get option :branch)
                :download (glof:get option :download t))))

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
