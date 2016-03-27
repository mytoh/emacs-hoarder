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
          (colle:map
           (lambda (key)
             ;; (format "%s: %s"
             ;;         (symbol-name s)
             ;;         (slot-value package s))
             (format "- %s :: %s"
                     (glof:string key)
                     (glof:get package key)))
           (glof:names package))
          "\n")))

(cl-defun hoarder:fetch (source &optional option)
  (declare (indent 1))
  (cl-letf* ((package (thread-first source
                        (hoarder:make-package option)
                        (hoarder:fetch-set-options option))))
    (hoarder:add-to-package-list package)
    (hoarder:option-info package)
    (hoarder:message "registered %s"    (glof:get package :name))
    (hoarder:message-fetch package)))

(cl-defun hoarder:fetch-has-option (option key)
  (glof:get option key nil))

(cl-defun hoarder:fetch-set-options (package option)
  (thread-first package
    (hoarder:fetch-set-option-compile option)
    (hoarder:fetch-set-option-depth option)))


(cl-defun hoarder:fetch-set-option-compile (package option)
  (if (not (hoarder:fetch-has-option option :compile))
      (glof:assoc package :compile nil)
    package))

(cl-defun hoarder:fetch-set-option-depth (package option)
  (if (not (hoarder:fetch-has-option option :depth))
      (glof:assoc package :depth 1)
    package))

(provide 'hoarder-fetch)

;;; fetch.el ends here

