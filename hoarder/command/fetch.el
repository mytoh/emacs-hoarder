;;; fetch -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

;;;; register

(cl-defmethod hoarder:message-fetch ((package hoarder:<package>))
  (hoarder:log (seq-concatenate 'string "\n* " (hoarder:package-name package) "\n%s")
              (string-join
               (seq-map
                (lambda (s)
                  ;; (format "%s: %s"
                  ;;         (symbol-name s)
                  ;;         (slot-value package s))
                  (let ((slot-symbol (eieio-slot-descriptor-name s)))
                    (format "- %s :: %s"
                            slot-symbol
                            (slot-value package slot-symbol))))
                (eieio-class-slots (eieio-object-class package)))
               "\n")))

(cl-defun hoarder:fetch (source &optional option)
  (cl-letf* ((package (hoarder:make-package source option)))
    (hoarder:fetch-set-options package option)
    (hoarder:add-to-package-list package)
    (hoarder:option-info package)
    (hoarder:message "registered %s"    (hoarder:package-name package))
    (hoarder:message-fetch package)))

(cl-defun hoarder:fetch-has-option (option key)
  (cl-getf option key nil))

(cl-defmethod hoarder:fetch-set-options ((package hoarder:<package>) option)
  (cond ((not (hoarder:fetch-has-option option :compile))
         (hoarder:fetch-set-option package :compile nil))))

(cl-defmethod hoarder:fetch-set-option ((package hoarder:<package>) slot value)
  (setf (slot-value package slot) value))

(provide 'hoarder-fetch)

;;; fetch.el ends here
