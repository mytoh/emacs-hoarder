;;; info -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-package "vendle/package")

(defmethod vendle:option-info ((package vendle:package))
  (cl-letf ((info (vendle:package-info package)))
    (cl-typecase info
      (list
       (cl-mapc
        (lambda (path)
          (vendle:option-info-set-infopath
           (expand-file-name path (vendle:package-path package))))
        (vendle:package-info package)))
      (string
       (vendle:option-info-set-infopath
        (expand-file-name info (vendle:package-path package)))))))

(cl-defun vendle:option-info-set-infopath (path)
  (setenv "INFOPATH"
          (concat path ":"
                  (getenv "INFOPATH"))))

(provide 'vendle-option-info)

;;; info.el ends here
