;;; git.el -*- lexical-binding: t -*-

(require 'seq)
(require 'glof)

(cl-defun hoarder:source-git-p (source)
  (pcase source
    ((pred (string-match (rx "git://")))
     t)
    ((pred (string-match (rx ".git" (zero-or-one "/") line-end)))
     t)
    (_ nil)))

(cl-defun hoarder:make-package-git (source option)
  (cl-letf ((name (hoarder:make-package-name-git source option))
            (path (hoarder:make-package-path-git source option))
            (lpath (hoarder:make-package-load-path-git source option))
            (compile (hoarder:make-package-compile-git source option))
            (origin (hoarder:make-package-origin-git source option)))
    (glof:plist :type :git
                :site ""
                :name name
                :path path
                :load-path lpath
                :url source
                :compile compile
                :dependency (glof:get option :dependency nil)
                :build (glof:get option :build nil)
                :info (glof:get option :info nil)
                :origin origin
                :tag (glof:get option :tag nil)
                :desc (glof:get option :desc "")
                :recursive (glof:get option :recursive)
                :branch (glof:get option :branch))))

(cl-defun hoarder:make-package-compile-git (source option)
  (if (glof:get option :build nil)
      nil
    (glof:get option :compile t)))

(cl-defun hoarder:make-package-name-git (source option)
  (if option
      (cl-letf ((name (glof:get option :name)))
        (if name
            name
          (file-name-base source)))
    (file-name-base source)))

(cl-defun hoarder:make-package-path-git (source option)
  (cl-letf ((p (glof:get option :path nil))
            (origin (hoarder:make-package-origin-git source option)))
    (if p
        (expand-file-name p hoarder-directory)
      (expand-file-name origin hoarder-directory))))

(cl-defun hoarder:make-package-load-path-git (source option)
  (cl-letf ((path (glof:get option :load-path nil))
            (origin (hoarder:make-package-origin-git source option)))
    (if (not (seq-empty-p path))
        (pcase path
          ((pred vectorp)
           (seq-into
            (seq-map
             (lambda (p)
               (hoarder:concat-path hoarder-directory origin p))
             path)
            'vector))
          ((pred stringp)
           (hoarder:concat-path hoarder-directory origin path)))
      (hoarder:concat-path hoarder-directory origin))))


(cl-defun hoarder:make-package-origin-git (source option)
  (cl-letf ((origin (glof:get option :origin nil)))
    (if origin
        origin
      (pcase source
        ((or (pred (string-match (rx "git://")))
             (pred (string-match (rx ".git" (zero-or-one "/") line-end))))
         (replace-regexp-in-string (rx (or (seq line-start "git://")
                                           (seq line-start "http://")
                                           (seq line-start "https://")
                                           (seq ".git" line-end)))
                                   "" source))
        (_ source))
      ;; (cond ((or (string-match (rx "git://") source)
      ;;            (string-match (rx ".git" (zero-or-one "/") line-end) source))
      ;;        (replace-regexp-in-string (rx (or (seq line-start "git://")
      ;;                                          (seq line-start "http://")
      ;;                                          (seq line-start "https://")
      ;;                                          (seq ".git" line-end)))
      ;;                                  "" source))
      ;;       (t source))
      )))

(provide 'hoarder-source-git)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
