;;; build -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-package "vendle/package")

(cl-defmethod vendle:option-build ((package vendle:package))
  (when (vendle:package-build package)
    (cl-letf ((commands (vendle:package-build package))
              (path (file-name-as-directory (vendle:package-path package))))
      (mapc
       (lambda (c)
         (cl-letf ((default-directory path))
           (shell-command c nil nil)))
       commands))))

(provide 'vendle-option-build)

;;; build.el ends here
