;;; compile -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)

(require 'hoarder-package "hoarder/package")

(cl-defmethod hoarder:option-compile ((package hoarder:<package>) path)
  (if (hoarder:package-compile package)
      (byte-recompile-directory path 0)))

(provide 'hoarder-option-compile)

;;; compile.el ends here
