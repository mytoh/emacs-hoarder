;;; util -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")

;;;; internal functions

(defconst hoarder:log-buffer-name
  "*hoarder log*")

(cl-defun hoarder:package-compare-fn (p1 p2)
  (and (cl-equalp (hoarder:package-name p1)
                  (hoarder:package-name p2))
       (cl-equalp (hoarder:package-origin p1)
                  (hoarder:package-origin p2))))

(cl-defmethod hoarder:installed? ((package hoarder:<package>))
  (and (file-exists-p (hoarder:package-path package))
       (seq-filter
        (lambda (p) (hoarder:package-compare-fn p package))
        hoarder:*packages*)))

(cl-defun hoarder:directory-git-p (p)
  (file-directory-p (expand-file-name ".git" p)))

(cl-defun hoarder:append-to-list (var elem)
  (if (listp elem)
      (seq-map
       (lambda (e) (add-to-list var e 'append))
       elem)
    (add-to-list var elem 'append)))

(cl-defun hoarder:add-to-list (var elem)
  (if (listp elem)
      (seq-map
       (lambda (e) (add-to-list var e))
       elem)
    (add-to-list var elem)))

(cl-defmethod hoarder:add-to-load-path ((package hoarder:<package>))
  (hoarder:add-to-list 'load-path (hoarder:package-load-path package)))

(cl-defmethod hoarder:add-to-theme-path ((package hoarder:<package>))
  (hoarder:add-to-list  'custom-theme-load-path (hoarder:package-load-path package)))

(cl-defmethod hoarder:add-to-package-list ((package hoarder:<package>))
  (hoarder:append-to-list  'hoarder:*packages* package))

;;;; utilily functions
(cl-defun hoarder:concat-path (&rest parts)
  (seq-reduce (lambda (a b) (expand-file-name b a)) parts
              "/"))

(cl-defun hoarder:message (fmt &rest texts)
  (if noninteractive
      (princ (apply #'format (concat fmt "\n") texts))
    (apply #'hoarder:log fmt texts)
    (apply #'message (format "[%s] %s"
                             (propertize "hoarder"
                                         'face '(:foreground "#539b8f"))
                             fmt)
           texts)))

(cl-defun hoarder:log (fmt &rest texts)
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
     (cl-find-if (lambda (v) (cl-equalp (hoarder:package-name v)
                                   p))
                 hoarder:*packages*))
   (seq-map
    (lambda (p) (format "%s" p))
    package-activated-list)))


(provide 'hoarder-util)

;;; util.el ends here
