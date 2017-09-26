;;; compile -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'glof)

(require 'hoarder-package "hoarder/package")

(cl-defun hoarder:option-compile (package path)
  (if (glof:get package :compile)
      (byte-recompile-directory path 0 'force)))

(provide 'hoarder-option-compile)

;;; compile.el ends here
