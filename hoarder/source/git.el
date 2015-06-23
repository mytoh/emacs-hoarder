;;; git.el -*- lexical-binding: t -*-

(require 'seq)

(cl-defun hoarder:source-git-p (source)
  (cond ((or (string-match (rx "git://") source)
             (string-match (rx ".git" (zero-or-one "/") line-end) source))
         t)
        (t nil)))

(cl-defun hoarder:make-package-git (source option)
  (cl-letf ((name (hoarder:make-package-name-git source option))
            (path (hoarder:make-package-path-git source option))
            (lpath (hoarder:make-package-load-path-git source option))
            (compile (hoarder:make-package-compile-git source option))
            (origin (hoarder:make-package-origin-git source option)))
    (make-instance 'hoarder:<package>
                   :type 'git
                   :site ""
                   :name name
                   :path path
                   :load-path lpath
                   :url source
                   :compile compile
                   :dependency (cl-getf option :depends nil)
                   :build (cl-getf option :build nil)
                   :info (cl-getf option :info nil)
                   :origin origin
                   :tag (cl-getf option :tag nil)
                   :desc (cl-getf option :desc "")
                   :recursive (cl-getf option :recursive)
                   :branch (cl-getf option :branch))))

(cl-defun hoarder:make-package-compile-git (source option)
  (if (cl-getf option :build nil)
      nil
    (cl-getf option :compile t)))

(cl-defun hoarder:make-package-name-git (source option)
  (if option
      (cl-letf ((name (cl-getf option :name)))
        (if name
            name
          (file-name-base source)))
    (file-name-base source)))

(cl-defun hoarder:make-package-path-git (source option)
  (cl-letf ((p (cl-getf option :path nil))
            (origin (hoarder:make-package-origin-git source option)))
    (if p
        (expand-file-name p hoarder-directory)
      (expand-file-name origin hoarder-directory))))

(cl-defun hoarder:make-package-load-path-git (source option)
  (cl-letf ((path (cl-getf option :load-path nil))
            (origin (hoarder:make-package-origin-git source option)))
    (if path
        (if (listp path)
            (seq-map
             (lambda (p)
               (hoarder:concat-path hoarder-directory origin p))
             path)
          (hoarder:concat-path hoarder-directory origin path))
      (hoarder:concat-path hoarder-directory origin))))


(cl-defun hoarder:make-package-origin-git (source option)
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

(provide 'hoarder-source-git)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
