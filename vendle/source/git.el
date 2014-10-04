;;; git.el -*- lexical-binding: t -*-

(cl-defun vendle:source-git-p (source)
  (cond ((or (string-match (rx "git://") source)
             (string-match (rx ".git" (zero-or-one "/") line-end) source))
         t)
        (t nil)))

(cl-defun vendle:make-package-git (source info)
  (cl-letf ((name (vendle:make-package-name-git source info))
            (path (vendle:make-package-path-git source info))
            (load-path (vendle:make-package-load-path-git source info))
            (compile (vendle:make-package-compile-git source info))
            (origin (vendle:make-package-origin-git source info)))
    (vendle:package name
                    :type 'git
                    :site ""
                    :name name
                    :path path
                    :load-path load-path
                    :url source
                    :compile compile
                    :deps (cl-getf info :deps nil)
                    :build (cl-getf info :build nil)
                    :info (cl-getf info :info nil)
                    :origin origin)))

(cl-defun vendle:make-package-compile-git (source info)
  (if (cl-getf info :build nil)
      nil
    (cl-getf info :compile t)))

(cl-defun vendle:make-package-name-git (source info)
  (if info
      (cl-letf ((name (cl-getf info :name)))
        (if name
            name
          (file-name-base source)))
    (file-name-base source)))

(cl-defun vendle:make-package-path-git (source info)
  (cl-letf ((p (cl-getf info :path nil))
            (origin (vendle:make-package-origin-git source info)))
    (if p
        (expand-file-name p vendle-directory)
      (expand-file-name origin vendle-directory))))

(cl-defun vendle:make-package-load-path-git (source info)
  (cl-letf ((path (cl-getf info :load-path nil))
            (origin (vendle:make-package-origin-git source info)))
    (if path
        (if (listp path)
            (cl-mapcar
             (lambda (p)
               (vendle:concat-path vendle-directory origin p))
             path)
          (vendle:concat-path vendle-directory origin path))
      (vendle:concat-path vendle-directory origin))))

;; (cl-defun vendle:make-package-load-path-git (source info)
;;   (cl-letf ((path (if info
;;                       (cl-letf ((path (cl-getf info :load-path nil))
;;                                 (name (vendle:make-package-origin-git source info)))
;;                         (if path
;;                             (if (listp path)
;;                                 (cl-mapcar
;;                                  (lambda (p) (cl-concatenate 'string
;;                                                         name  "/" p))
;;                                  path)
;;                               (cl-concatenate 'string
;;                                               name  "/"  path))
;;                           name))
;;                     (vendle:make-package-name-git source info))))
;;     (if (listp path)
;;         (cl-mapcar
;;          (lambda (p) (expand-file-name p vendle-directory))
;;          path)
;;       (expand-file-name path vendle-directory))))

(cl-defun vendle:make-package-origin-git (source info)
  (cl-letf ((origin (cl-getf info :origin nil)))
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
