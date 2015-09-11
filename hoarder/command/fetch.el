;;; fetch -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

;;;; register

(cl-defun hoarder:message-fetch (package)
  (hoarder:log (seq-concatenate 'string "\n* " (glof:get package :name) "\n%s")
               (string-join
                (seq-map
                 (lambda (key)
                   ;; (format "%s: %s"
                   ;;         (symbol-name s)
                   ;;         (slot-value package s))
                   (format "- %s :: %s"
                           (glof:stringify key)
                           (glof:get package key)))
                 (glof:keys package))
                "\n")))

(cl-defun hoarder:fetch (source &optional option)
  (cl-letf* ((package (thread-first source
                        (hoarder:make-package option)
                        (hoarder:fetch-set-options option))))
    (hoarder:add-to-package-list package)
    (hoarder:option-info package)
    (hoarder:message "registered %s"    (glof:get package :name))
    (hoarder:message-fetch package)))

(cl-defun hoarder:fetch-has-option (option key)
  (glof:get option key nil))

(cl-defmethod hoarder:fetch-set-options (package option)
  (cond ((not (hoarder:fetch-has-option option :compile))
         (hoarder:fetch-set-option package :compile nil))))

(cl-defmethod hoarder:fetch-set-option (package key value)
  (glof:assoc package key value))

(provide 'hoarder-fetch)

;;; fetch.el ends here
