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
            (compile (vendle:make-package-compile-git source info)))
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
                    :info (cl-getf info :info nil))))

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
  (cl-letf ((path (if info
                      (cl-letf ((p (cl-getf info :path))
                                (name (vendle:make-package-name-git source info)))
                        (if p
                            (cl-concatenate 'string
                                            name "/" p)
                          name))
                    (vendle:make-package-name-git source info))))
    (expand-file-name path vendle-directory)))

(cl-defun vendle:make-package-load-path-git (source info)
  (cl-letf ((path (if info
                      (cl-letf ((path (cl-getf info :load-path))
                                (name (vendle:make-package-name-git source info)))
                        (if path
                            (if (listp path)
                                (cl-mapcar
                                 (lambda (p) (cl-concatenate 'string
                                                        name  "/" p))
                                 path)
                              (cl-concatenate 'string
                                              name  "/"  path))
                          name))
                    (vendle:make-package-name-git source info))))
    (if (listp path)
        (cl-mapcar
         (lambda (p) (expand-file-name p vendle-directory))
         path)
      (expand-file-name path vendle-directory))))

(provide 'vendle-source-git)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
