;;; fetch -*- lexical-binding: t; coding: utf-8; -*-

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

(cl-defun vendle:fetch (source &optional option)
  (cl-letf* ((package (vendle:make-package source option)))
    (vendle:fetch-set-options package option)
    (vendle:add-to-package-list package)
    (vendle:option-info package)
    (vendle:message "registered %s"    (vendle:package-name package))))

(cl-defun vendle:fetch-has-option (option key)
  (cl-getf option key nil))

(defmethod vendle:fetch-set-options ((package vendle:package) option)
  (cond ((not (vendle:fetch-has-option option :compile))
         (vendle:fetch-set-option package :compile nil))))

(defmethod vendle:fetch-set-option ((package vendle:package) slot value)
    (set-slot-value package slot value))

(provide 'vendle-fetch)

;;; fetch.el ends here
