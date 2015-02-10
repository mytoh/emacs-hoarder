;;; git.el -*- lexical-binding: t -*-

(require 'seq)

(cl-defun vendle:source-git-p (source)
  (cond ((or (string-match (rx "git://") source)
             (string-match (rx ".git" (zero-or-one "/") line-end) source))
         t)
        (t nil)))

(cl-defun vendle:make-package-git (source option)
  (cl-letf ((name (vendle:make-package-name-git source option))
            (path (vendle:make-package-path-git source option))
            (lpath (vendle:make-package-load-path-git source option))
            (compile (vendle:make-package-compile-git source option))
            (origin (vendle:make-package-origin-git source option)))
    (make-instance 'vendle:package
                   :type 'git
                   :site ""
                   :name name
                   :path path
                   :load-path lpath
                   :url source
                   :compile compile
                   :deps (cl-getf option :deps nil)
                   :build (cl-getf option :build nil)
                   :info (cl-getf option :info nil)
                   :origin origin
                   :tag (cl-getf option :tag nil)
                   :desc (cl-getf option :desc "")
                   :recursive (cl-getf option :recursive))))

(cl-defun vendle:make-package-compile-git (source option)
  (if (cl-getf option :build nil)
      nil
    (cl-getf option :compile t)))

(cl-defun vendle:make-package-name-git (source option)
  (if option
      (cl-letf ((name (cl-getf option :name)))
        (if name
            name
          (file-name-base source)))
    (file-name-base source)))

(cl-defun vendle:make-package-path-git (source option)
  (cl-letf ((p (cl-getf option :path nil))
            (origin (vendle:make-package-origin-git source option)))
    (if p
        (expand-file-name p vendle-directory)
      (expand-file-name origin vendle-directory))))

(cl-defun vendle:make-package-load-path-git (source option)
  (cl-letf ((path (cl-getf option :load-path nil))
            (origin (vendle:make-package-origin-git source option)))
    (if path
        (if (listp path)
            (seq-map
             (lambda (p)
               (vendle:concat-path vendle-directory origin p))
             path)
          (vendle:concat-path vendle-directory origin path))
      (vendle:concat-path vendle-directory origin))))


(cl-defun vendle:make-package-origin-git (source option)
  (cl-letf ((origin (cl-getf option :origin nil)))
    (if origin
        origin
      (cond ((or (string-match (rx "git://") source)
                 (string-match (rx ".git" (zero-or-one "/") line-end) source))
             (replace-regexp-in-string (rx (or (seq line-start "git://")
                                               (seq line-start "http://")
                                               (seq line-start "https://")
                                               (seq ".git" line-end)))
                                       "" source))
            (t source)))))

(provide 'vendle-source-git)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
