;;; record -- record -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

(cl-defun hoarder:message-record (package)
  (hoarder:log (seq-concatenate 'string "\n* " (glof:get package :name) "\n%s")
         (string-join
          (colle:map
           (lambda (key)
             (format "- %s :: %s"
                     (glof:string key)
                     (glof:get package key)))
           (glof:names package))
          "\n")))

(cl-defun hoarder:record (source &optional option)
  (declare (indent 1))
  (cl-letf* ((package (thread-first source
                        (hoarder:make-package option)
                        (hoarder:record-set-options option))))
    (hoarder:add-to-package-list package)
    (hoarder:option-info package)
    (unless noninteractive
      (hoarder:message "registered %s"    (glof:get package :name)))
    (hoarder:message-record package)))

(cl-defun hoarder:record-set-options (package option)
  (thread-first package
    (hoarder:record-set-option-compile option)
    (hoarder:record-set-option-depth option)
    (hoarder:record-set-option-download option)))

(cl-defun hoarder:record-set-option-compile (package option)
  (pcase (glof:lookup :compile option)
    (`[:nothing]
     (glof:assoc package :compile nil))
    (`[:just ,v]
     (glof:assoc package :compile v))))

(cl-defun hoarder:record-set-option-depth (package option)
  (pcase (glof:lookup :depth option)
    (`[:nothing]
     (glof:assoc package :depth 1))
    (`[:just ,v]
     (glof:assoc package :depth v))))

(cl-defun hoarder:record-set-option-download (package option)
  (pcase (glof:lookup :download option)
    (`[:nothing]
     (glof:assoc package :download nil))
    (`[:just ,v]
     (glof:assoc package :download v))))

(provide 'hoarder-record)

;;; record.el ends here


;;; record.el ends here
