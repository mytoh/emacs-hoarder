;;; register -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")
(require 'vendle-option "vendle/option")

;;;; register

(cl-defun vendle:register (_source &optional option)
  (cl-letf* ((package (vendle:make-package _source option)))
    (vendle:resolve-deps package)
    (vendle:add-to-load-path package)
    (vendle:add-to-package-list package)
    (vendle:option-info package)
    (vendle:message "registered %s"    (vendle:package-name package))))

(cl-defun vendle:register-local (source &optional option)
  (cl-letf* ((path (expand-file-name source))
             (package (vendle:make-package-local path option)))
    (vendle:add-to-load-path package)
    (vendle:add-to-package-list package)
    (vendle:option-info package)
    (vendle:message "registered %s locally"
                    (vendle:package-name package))))

(cl-defun vendle:register-theme (source &optional option)
  (cl-letf* ((package (vendle:make-package source option)))
    (vendle:add-to-theme-path package)
    (vendle:add-to-load-path package)
    (vendle:add-to-package-list package)
    (vendle:message "registered %s as theme"
                    (vendle:package-name package))))

(cl-defun vendle:register-theme-local (_source &optional option)
  (cl-letf* ((path (expand-file-name _source))
             (package (vendle:make-package-local path option)))
    (vendle:add-to-theme-path package)
    (vendle:add-to-package-list package)
    (vendle:message "registered %s as local theme"
                    (vendle:package-name package))))

(defmethod vendle:resolve-deps ((package vendle:package))
  (if-let ((deps (vendle:package-deps package)))
      (cl-mapc
       'vendle:install-dep
       deps)
    nil))

(cl-defun vendle:install-dep (deps)
  (cl-typecase deps
    (list
     (vendle:register (car deps)
                      (cdr deps)))
    (string
     (vendle:register deps nil))))

(provide 'vendle-register)

;;; register.el ends here
