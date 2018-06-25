;;; gitlab -- gitlab -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:



(require 'seq)
(require 'colle)
(require 'rx)

(cl-defun hoarder:source-site-gitlab-p (source)
  (pcase source
    ((rx "gitlab:" (submatch (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))))
     t)
    ((rx "gitlab:" (submatch (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))))
     t)
    (_ nil)))

(cl-defun hoarder:source-site-format-gitlab (source)
  (pcase source
    ((rx "gitlab:" (submatch (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))))
     (match-string-no-properties 1 source))
    ((rx "gitlab:" (submatch (+ (not (in "/")))
                             "/"
                             (+ (not (in "/")))))
     (match-string-no-properties 1 source))))

(cl-defun hoarder:make-package-gitlab (source option)
  (cl-letf ((name (hoarder:make-package-name-gitlab source option))
            (path (hoarder:make-package-path-gitlab source option))
            (lpath (hoarder:make-package-load-path-gitlab source option))
            (origin (hoarder:make-package-origin-gitlab source option)))
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
     :site "gitlab"
     :tags (glof:get option :tags nil)
     :type :git
     :url (seq-concatenate 'string "https://" origin))))

(cl-defun hoarder:make-package-name-gitlab (source option)
  (pcase option
    (`nil (car (last (split-string source "/"))))
    (_
     (pcase (glof:get option :name)
       (`nil (car (last (split-string source "/"))))
       (name name)))))

(cl-defun hoarder:make-package-load-path-gitlab (source option)
  (cl-letf ((origin (hoarder:make-package-origin-gitlab source option)))
    (if option
        (cl-letf ((path (glof:get option :load-path nil)))
          (pcase path
            ((pred seq-empty-p)
             (hoarder:concat-path hoarder-directory
                            (string-trim origin nil "\\.git")))
            ((pred vectorp)
             (colle:map
              (lambda (p)
                (hoarder:concat-path hoarder-directory origin p))
              path))
            ((pred stringp)
             (hoarder:concat-path hoarder-directory origin path))))        
      (hoarder:concat-path hoarder-directory
                     (string-trim origin nil "\\.git"))))) 

(cl-defun hoarder:make-package-path-gitlab (source option)
  (cl-letf ((path (glof:get option :path))
            (origin (hoarder:make-package-origin-gitlab source option)))
    (if path
        (hoarder:concat-path hoarder-directory path)
      (hoarder:concat-path hoarder-directory
                     (string-trim origin nil "\\.git")))))

(cl-defun hoarder:make-package-origin-gitlab (source option)
  (cl-letf ((origin (glof:get option :origin nil)))
    (if origin
        origin
      (pcase source
        ((rx line-start
             (submatch (one-or-more (not (in "/"))))
             "/"
             (submatch (one-or-more (not (in "/"))))
             "/"
             (submatch (one-or-more (not (in "/"))))
             line-end)
         (seq-concatenate 'string
                          "gitlab.com/"
                          (match-string-no-properties 1 source)
                          "/"
                          (match-string-no-properties 2 source)
                          "/"
                          (match-string-no-properties 3 source)
                          ".git"))
        ((rx line-start
             (submatch (one-or-more (not (in "/"))))
             "/"
             (submatch (one-or-more (not (in "/"))))
             line-end)
         (seq-concatenate 'string
                          "gitlab.com/"
                          (match-string-no-properties 1 source)
                          "/"
                          (match-string-no-properties 2 source)
                          ".git"))))))

(provide 'hoarder-source-gitlab)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; gitlab.el ends here
