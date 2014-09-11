;;; info -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-package "vendle/package")

(defmethod vendle:option-info ((package vendle:package))
  (cl-typecase (vendle:package-info package)
    (list
     (cl-mapc
      (lambda (path)
        (setenv "INFOPATH"
                (concat (concat (expand-file-name path (vendle:package-path package)) ":")
                        (getenv "INFOPATH"))))
      (vendle:package-info package)))))

(provide 'vendle-option-info)

;;; info.el ends here
