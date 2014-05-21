;;; register -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; register

(cl-defun vendle:register (_source &optional _info)
  (cl-letf* ((package (vendle:make-package _source _info)))
    (vendle:resolve-deps package)
    (vendle:add-to-load-path package)
    (vendle:add-to-package-list package)
    (vendle:message "registered %s"    (vendle:package-name package))))

(cl-defun vendle:register-local (source &optional info)
  (cl-letf* ((path (expand-file-name source))
             (package (vendle:make-package-local path info)))
    (vendle:add-to-load-path package)
    (vendle:add-to-package-list package)
    (vendle:message "registered %s locally"
                    (vendle:package-name package))))

(cl-defun vendle:register-theme (source &optional info)
  (cl-letf* ((package (vendle:make-package source info)))
    (vendle:add-to-theme-path package)
    (vendle:add-to-package-list package)
    (vendle:message "registered %s as theme"
                    (vendle:package-name package))))

(cl-defun vendle:register-theme-local (_source &optional _info)
  (cl-letf* ((path (expand-file-name _source))
             (package (vendle:make-package-local path _info)))
    (vendle:add-to-theme-path package)
    (vendle:add-to-package-list package)
    (vendle:message "registered %s as local theme"
                    (vendle:package-name package))))

(defmethod vendle:resolve-deps ((package vendle:package))
  (cl-letf ((deps (vendle:package-deps package)))
    (if deps
        (cl-mapc
         'vendle:install-dep
         deps)
      nil)))

(cl-defun vendle:install-dep (information)
  (cl-typecase information
    (list
     (vendle:register (car information)
                      (cdr information)))
    (string
     (vendle:register information nil))))

(provide 'vendle-register)

;;; register.el ends here
