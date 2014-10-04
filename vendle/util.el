;;; util -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")

;;;; internal functions

(cl-defun vendle:directory-git-p (p)
  (if (file-directory-p (expand-file-name ".git" p))
      t nil))

(cl-defun vendle:append-to-list (var elem)
  (if (listp elem)
      (cl-mapcar
       (lambda (e) (add-to-list var e 'append))
       elem)
    (add-to-list var elem 'append)))

(cl-defun vendle:add-to-list (var elem)
  (if (listp elem)
      (cl-mapcar
       (lambda (e) (add-to-list var e))
       elem)
    (add-to-list var elem)))

(defmethod vendle:add-to-load-path ((package vendle:package))
  (vendle:add-to-list 'load-path (vendle:package-load-path package)))

(defmethod vendle:add-to-theme-path ((package vendle:package))
  (vendle:add-to-list  'custom-theme-load-path (vendle:package-load-path package)))

(defmethod vendle:add-to-package-list ((package vendle:package))
  (vendle:append-to-list  '*vendle-package-list* package))

;;;; utilily functions
(cl-defun vendle:concat-path (&rest parts)
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

(cl-defun vendle:message (fmt &rest text)
  (with-current-buffer (get-buffer-create "*vendle log*")
    (goto-char (point-max))
    (insert (apply 'format (format "[%s] %s"
                                   (propertize "vendle"
                                               'face '(:foreground "#539b8f"))
                                   fmt)
                   text))
    (insert "\n"))
  (apply 'message (format "[%s] %s"
                          (propertize "vendle"
                                      'face '(:foreground "#539b8f"))
                          fmt)
         text))

(cl-defun vendle:foreach-package-list (fn)
  (cl-mapc fn *vendle-package-list*))

(provide 'vendle-util)

;;; util.el ends here
