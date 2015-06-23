;;; github.el -*- lexical-binding: t -*-

(require 'seq)

(cl-defun hoarder:source-site-github-p (source)
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

(cl-defun hoarder:source-site-format-github (source)
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

(cl-defun hoarder:make-package-github (source option)
  (cl-letf ((name (hoarder:make-package-name-github source option))
            (path (hoarder:make-package-path-github source option))
            (lpath (hoarder:make-package-load-path-github source option))
            (origin (hoarder:make-package-origin-github source option)))
    (make-instance 'hoarder:<package>
                   :type 'git
                   :site "github"
                   :name name
                   :path path
                   :load-path lpath
                   :url (seq-concatenate 'string "git@github.com:" source)
                   :compile (cl-getf option :compile t)
                   :dependency (cl-getf option :depends nil)
                   :build (cl-getf option :build nil)
                   :info (cl-getf option :info nil)
                   :origin origin
                   :tag (cl-getf option :tag nil)
                   :desc (cl-getf option :desc "")
                   :recursive (cl-getf option :recursive)
                   :branch (cl-getf option :branch))))

(cl-defun hoarder:make-package-name-github (source option)
  (if option
      (cl-letf ((name (cl-getf option :name)))
        (if name
            name
          (cl-second (split-string source "/"))))
    (cl-second (split-string source "/"))))

(cl-defun hoarder:make-package-load-path-github (source option)
  (cl-letf ((path (cl-getf option :load-path nil))
            (origin (hoarder:make-package-origin-github source option)))
    (if path
        (if (listp path)
            (seq-map
             (lambda (p)
               (hoarder:concat-path hoarder-directory origin p))
             path)
          (hoarder:concat-path hoarder-directory origin path))
      (hoarder:concat-path hoarder-directory origin))))

(cl-defun hoarder:make-package-path-github (source option)
  (cl-letf ((path (cl-getf option :path))
            (origin (hoarder:make-package-origin-github source option)))
    (if path
        (hoarder:concat-path hoarder-directory path)
      (hoarder:concat-path hoarder-directory origin))))

(cl-defun hoarder:make-package-origin-github (source option)
  (cl-letf ((origin (cl-getf option :origin nil)))
    (if origin
        origin
      (cond
        ((string-match (rx line-start
                           "github:"
                           (submatch (one-or-more (not (in "/"))))
                           "/"
                           (submatch (one-or-more (not (in "/"))))
                           line-end)
                       source)
         (seq-concatenate 'string
                          "github.com/"
                          (match-string-no-properties 1 source)
                          "/"
                          (match-string-no-properties 2 source)))
        ((string-match (rx   line-start
                             (submatch (one-or-more (not (in "/"))))
                             "/"
                             (submatch (one-or-more (not (in "/"))))
                             line-end)
                       source)
         (seq-concatenate 'string
                          "github.com/"
                          (match-string-no-properties 1 source)
                          "/"
                          (match-string-no-properties 2 source)))))))

(provide 'hoarder-source-github)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
