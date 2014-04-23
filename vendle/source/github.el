;;; github.el -*- lexical-binding: t -*-

(cl-defun vendle:source-site-github-p (source)
  (cond
    ((string-match (rx "github:" (submatch (+ (not (in "/")))
                                           "/"
                                           (+ (not (in "/")))))
                   source)
     t)
    ((string-match (rx   line-start
                         (one-or-more (not (in "/")))
                         "/"
                         (one-or-more (not (in "/")))
                         line-end)
                   source)
     t)
    (t nil)))

(cl-defun vendle:source-site-format-github (source)
  (cond
    ((string-match (rx "github:" (submatch (+ (not (in "/")))
                                           "/"
                                           (+ (not (in "/")))))
                   source)
     (match-string-no-properties 1 source))
    ((string-match (rx   line-start
                         (one-or-more (not (in "/")))
                         "/"
                         (one-or-more (not (in "/")))
                         line-end)
                   source)
     (match-string-no-properties 0 source))))

(cl-defun vendle:make-package-github (source info)
  (cl-letf ((name (vendle:make-package-name-github source info))
            (path (vendle:make-package-path-github source info))
            (load-path (vendle:make-package-load-path-github source info)))
    (vendle:package name
                    :type 'git
                    :site "github"
                    :name name
                    :path path
                    :load-path load-path
                    :url (cl-concatenate 'string "git://github.com/" source)
                    :compile (if info
                                 (cl-getf info :compile)
                               t))))

(cl-defun vendle:make-package-name-github (source info)
  (if info
      (cl-letf ((name (cl-getf info :name)))
        (if name
            name
          (cadr (split-string source "/"))))
    (cadr (split-string source "/"))))

(cl-defun vendle:make-package-load-path-github (source info)
  (cl-letf ((path (if info
                      (cl-letf ((path (cl-getf info :load-path))
                                (name (vendle:make-package-name-github source info)))
                        (if path
                            (cl-concatenate 'string
                                            name  "/"  path)
                          name))
                    (vendle:make-package-name-github source info))))
    (expand-file-name path vendle-directory)))

(cl-defun vendle:make-package-path-github (source info)
  (cl-letf ((path (if info
                      (cl-letf ((path (cl-getf info :path))
                                (name (vendle:make-package-name-github source info)))
                        (if path
                            (cl-concatenate 'string
                                            name  "/"  path)
                          name))
                    (vendle:make-package-name-github source info))))
    (expand-file-name path vendle-directory)))

(provide 'vendle-source-github)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
