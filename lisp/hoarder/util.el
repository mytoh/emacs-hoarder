;;; util -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(require 'glof)
(require 'colle)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")

;;;; internal functions

(defconst hoarder:log-buffer-name
  "*hoarder log*")

(cl-defun hoarder:on (g f x y)
  ;; [[https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Function.html#v:on]]
  (funcall g
     (funcall f x)
     (funcall f y)))

(cl-defun hoarder:package-compare-fn (p1 p2)
  (cl-labels ((comp (n)
                    (hoarder:on #'cl-equalp
                          (lambda (p) (glof:get p n))
                          p1
                          p2)))
    (and (comp :name)
       (comp :origin))))

(cl-defun hoarder:installed? (package)
  (and (file-exists-p (glof:get package :path))
     (pcase (seq-filter
         (lambda (p) (hoarder:package-compare-fn p package))
         hoarder:*packages*)
       ((pred colle:empty-p)
        nil)
       (_ t))))

(cl-defun hoarder:directory-git-p (p)
  (file-directory-p (expand-file-name ".git" p)))

(cl-defun hoarder:append-to-list (var elem)
  (add-to-list var elem))

(cl-defun hoarder:add-to-list (var elem)
  (pcase elem
    ((pred vectorp)
     (colle:map
      ;; seq-each
      (lambda (e) (add-to-list var e))
      elem))
    (_
     (add-to-list var elem))))

(cl-defun hoarder:add-to-load-path (package)
  (hoarder:add-to-list 'load-path (glof:get package :load-path)))

(cl-defun hoarder:add-to-theme-path (package)
  (hoarder:add-to-list  'custom-theme-load-path (glof:get package :load-path)))

(cl-defun hoarder:add-to-package-list (package)
  (setq hoarder:*packages*
        (colle:conj hoarder:*packages* package)))

;;;; utilily functions
(cl-defun hoarder:concat-path (&rest parts)
  (colle:foldl (lambda (a b) (expand-file-name b a))
            "/"
            parts))

(cl-defun hoarder:message (fmt . texts)
  (if noninteractive
      (princ (apply #'format (concat fmt "\n") texts))
    (apply #'hoarder:log fmt texts)
    (apply #'message (format "[%s] %s"
                             (propertize "hoarder"
                                         'face '(:foreground "#539b8f"))
                             fmt)
           texts)))

(cl-defun hoarder:log (fmt . texts)
  (with-current-buffer (get-buffer-create hoarder:log-buffer-name)
    (when (hoarder:empty-buffer-p (current-buffer))
      (goto-char (point-min))
      (insert ";; -*- mode: org -*-\n"))
    (goto-char (point-max))
    (insert (apply #'format fmt texts))
    (insert "\n")))

(cl-defun hoarder:foreach-package-list (fn)
  (seq-each fn hoarder:*packages*))

(cl-defun hoarder:empty-buffer-p (buffer)
  "Check if BUFFER have contens."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(cl-defun hoarder:find-duplicate-packages ()
  (seq-filter
   (lambda (p)
     (seq-find
      (lambda (v) (cl-equalp (glof:get v :name)
                        p))
      hoarder:*packages*))
   (seq-map
    (lambda (p) (format "%s" p))
    package-activated-list)))

(provide 'hoarder-util)

;;; util.el ends here
