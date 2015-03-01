;;; build -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)

(require 'seq)
(require 'vendle-package "vendle/package")

(cl-defmethod vendle:option-build ((package vendle:<package>))
  (when (vendle:package-build package)
    (cl-letf ((commands (vendle:package-build package))
              (path (file-name-as-directory (vendle:package-path package))))
      (seq-each
       (lambda (c)
         (cl-letf ((default-directory path))
           (shell-command c nil nil)))
       commands))))

(provide 'vendle-option-build)

;;; build.el ends here
