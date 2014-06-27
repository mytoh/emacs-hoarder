;;; compile -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-package "vendle/package")

(defmethod vendle:option-compile ((package vendle:package) path)
  (if (vendle:package-compile package)
      (byte-recompile-directory path 0)))

(provide 'vendle-option-compile)

;;; compile.el ends here
