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
    (glof:plist :type 'git
                :site "github"
                :name name
                :path path
                :load-path lpath
                :url (seq-concatenate 'string "git@github.com:" source)
                :compile (glof:get option :compile t)
                :dependency (glof:get option :dependency nil)
                :build (glof:get option :build nil)
                :info (glof:get option :info nil)
                :origin origin
                :tag (glof:get option :tag nil)
                :desc (glof:get option :desc "")
                :recursive (glof:get option :recursive)
                :branch (glof:get option :branch))))

(cl-defun hoarder:make-package-name-github (source option)
  (if option
      (cl-letf ((name (glof:get option :name)))
        (if name
            name
          (cl-second (split-string source "/"))))
    (cl-second (split-string source "/"))))

(cl-defun hoarder:make-package-load-path-github (source option)
  (cl-letf ((origin (hoarder:make-package-origin-github source option)))
    (if option
        (cl-letf ((path (glof:get option :load-path nil)))
          (pcase path
            ((pred vectorp)
             (seq-into
              (seq-map
               (lambda (p)
                 (hoarder:concat-path hoarder-directory origin p))
               path)
              'vector))
            ((pred stringp)
             (hoarder:concat-path hoarder-directory origin path))
            ((pred seq-empty-p)
             (hoarder:concat-path hoarder-directory origin))))        
      (hoarder:concat-path hoarder-directory origin)))) 

(cl-defun hoarder:make-package-path-github (source option)
  (cl-letf ((path (glof:get option :path))
            (origin (hoarder:make-package-origin-github source option)))
    (if path
        (hoarder:concat-path hoarder-directory path)
      (hoarder:concat-path hoarder-directory origin))))

(cl-defun hoarder:make-package-origin-github (source option)
  (cl-letf ((origin (glof:get option :origin nil)))
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
