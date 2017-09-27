;;; github.el -*- lexical-binding: t -*-

(require 'seq)
(require 'colle)

(cl-defun hoarder:source-site-github-p (source)
  (pcase source
    ((or (rx "github:" (submatch (+ (not (in "/")))
                                "/"
                                (+ (not (in "/")))))
        (rx   line-start
              (one-or-more (not (in "/")))
              "/"
              (one-or-more (not (in "/")))
              line-end))
     t)
    (_ nil)))

(cl-defun hoarder:source-site-format-github (source)
  (pcase source
    ((rx "github:" (submatch (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))))
     (match-string-no-properties 1 source))
    ((rx   line-start
           (one-or-more (not (in "/")))
           "/"
           (one-or-more (not (in "/")))
           line-end)
     (match-string-no-properties 0 source))))

(cl-defun hoarder:make-package-github (source option)
  (cl-letf ((name (hoarder:make-package-name-github source option))
            (path (hoarder:make-package-path-github source option))
            (lpath (hoarder:make-package-load-path-github source option))
            (origin (hoarder:make-package-origin-github source option)))
    (glof:plist
     :branch (glof:get option :branch)
     :build (glof:get option :build nil)
     :compile (glof:get option :compile t)
     :dependencies (glof:get option :dependencies nil)
     :description (glof:get option :description "")
     :download (glof:get option :download t)
     :info (glof:get option :info nil)
     :load-path lpath
     :name name
     :origin origin
     :path path
     :recursive (glof:get option :recursive)
     :site "github"
     :tags (glof:get option :tags nil)
     :type :git
     :url (seq-concatenate 'string "https://github.com/" source))))

(cl-defun hoarder:make-package-name-github (source option)
  (pcase option
    (`nil (cl-second (split-string source "/")))
    (_
     (pcase (glof:get option :name)
       (`nil (cl-second (split-string source "/")))
       (name name)))))

(cl-defun hoarder:make-package-load-path-github (source option)
  (cl-letf ((origin (hoarder:make-package-origin-github source option)))
    (if option
        (cl-letf ((path (glof:get option :load-path nil)))
          (pcase path
            ((pred seq-empty-p)
             (hoarder:concat-path hoarder-directory origin))
            ((pred vectorp)
             (colle:map
              (lambda (p)
                (hoarder:concat-path hoarder-directory origin p))
              path))
            ((pred stringp)
             (hoarder:concat-path hoarder-directory origin path))))        
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
      (pcase source
        ((rx line-start
             "github:"
             (submatch (one-or-more (not (in "/"))))
             "/"
             (submatch (one-or-more (not (in "/"))))
             line-end)
         (seq-concatenate 'string
                          "github.com/"
                          (match-string-no-properties 1 source)
                          "/"
                          (match-string-no-properties 2 source)))
        ((rx   line-start
               (submatch (one-or-more (not (in "/"))))
               "/"
               (submatch (one-or-more (not (in "/"))))
               line-end)
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
