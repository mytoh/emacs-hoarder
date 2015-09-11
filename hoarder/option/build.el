                                        ;/bin/tcsh;; build -*- lexical-binding: t -*-

;;; Code:

;;;; requirements
(require 'cl-lib)
(require 'eieio)

(require 'seq)
(require 'hoarder-package "hoarder/package")

(cl-defun hoarder:option-build (package)
  (when (glof:get package :build)
    (cl-letf ((commands (glof:get package :build))
              (path (file-name-as-directory (glof:get package :path))))
      (seq-each
       (lambda (c)
         (cl-letf ((default-directory path))
           (shell-command c nil nil)))
       commands))))

(provide 'hoarder-option-build)

;;; build.el ends here
