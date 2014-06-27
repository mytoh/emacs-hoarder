;;; build -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-package "vendle/package")

(defmethod vendle:option-build ((package vendle:package))
  (when (vendle:package-build package)
    (setq old-dir default-directory)
    (cl-letf ((commands (vendle:package-build package))
              (path (vendle:package-path package)))
      (mapc
       (lambda (c)
         (cd path)
         (shell-command c nil nil))
       commands))
    (setq default-directory old-dir)))

(provide 'vendle-option-build)

;;; build.el ends here
