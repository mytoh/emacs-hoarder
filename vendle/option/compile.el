;;; compile -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)

(require 'vendle-package "vendle/package")

(cl-defmethod vendle:option-compile ((package vendle:<package>) path)
  (if (vendle:package-compile package)
      (byte-recompile-directory path 0)))

(provide 'vendle-option-compile)

;;; compile.el ends here
