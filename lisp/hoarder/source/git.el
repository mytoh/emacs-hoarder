;;; git.el -*- lexical-binding: t -*-

(require 'seq)

(require 'glof)
(require 'colle)

(cl-defun hoarder:trim-protocol (s)
  (pcase s
    ((rx (seq bol (+ alpha) "://"))
     (replace-regexp-in-string  "^[a-z]+://" "" s t t))
    (_ s)))

(cl-defun hoarder:source-git-p (source)
  (pcase source
    ((or (rx "http" (? (any "s")) "://git." )
         (rx "git://")
         (rx ".git" (zero-or-one "/") line-end))
     t)
    (_ nil)))

(cl-defun hoarder:make-package-git (source option)
  (cl-letf ((name (hoarder:make-package-name-git source option))
            (path (hoarder:make-package-path-git source option))
            (lpath (hoarder:make-package-load-path-git source option))
            (compile (hoarder:make-package-compile-git source option))
            (origin (hoarder:make-package-origin-git source option)))
    (glof:plist
     :branch (glof:get option :branch)
     :build (glof:get option :build nil)
     :compile compile
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
     :type :git
     :url source)))

(cl-defun hoarder:make-package-compile-git (_source option)
  (if (glof:get option :build nil)
      nil
    (glof:get option :compile t)))

(cl-defun hoarder:make-package-name-git (source option)
  (pcase option
    (`nil (file-name-base source))
    (_
     (cl-letf ((name (glof:get option :name)))
       (if name
           name
         (file-name-base source))))))

(cl-defun hoarder:make-package-path-git (source option)
  (cl-letf ((p (glof:get option :path nil))
            (origin (hoarder:trim-protocol source)))
    (if p
        (expand-file-name p hoarder-directory)
      (expand-file-name origin hoarder-directory))))

(cl-defun hoarder:make-package-load-path-git (source option)
  (cl-letf ((path (glof:get option :load-path nil))
            (origin (hoarder:trim-protocol source)))
    (pcase path
      ((or `[] `nil)
       (hoarder:concat-path hoarder-directory origin))
      ((pred vectorp)
       (colle:map
        (lambda (p)
          (hoarder:concat-path hoarder-directory origin p))
        path))
      ((pred stringp)
       (hoarder:concat-path hoarder-directory origin path)))))


(cl-defun hoarder:make-package-origin-git (source option)
  (cl-letf ((origin (glof:get option :origin nil)))
    (if origin
        origin
      (pcase source
        ((or (rx "git://")
            (rx ".git" (zero-or-one "/") line-end))
         (replace-regexp-in-string (rx (or (seq line-start "git://")
                                          (seq line-start "http://")
                                          (seq line-start "https://")
                                          (seq ".git" line-end)))
                                   "" source))
        (_ source)))))

(provide 'hoarder-source-git)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
