;;; register -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

;;;; register

(cl-defun hoarder:register (source &optional option)
  (hoarder:handle-register source option))

(cl-defun hoarder:handle-register (source option)
  (cond
    ((and (file-name-absolute-p source)
          (file-exists-p source))
     (hoarder:register-local source option))
    (t
     (hoarder:register-register source option))))

(cl-defmethod hoarder:message-register ((package hoarder:<package>))
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

(cl-defun hoarder:register-register (source &optional option)
  (cl-letf* ((package (hoarder:make-package source option)))
    (hoarder:resolve-deps package)
    (unless (hoarder:installed? package)
      (hoarder:add-to-load-path package)
      (hoarder:add-to-package-list package)
      (hoarder:option-info package)
      (hoarder:message "registered %s"  (hoarder:package-name package))
      (hoarder:message-register package)
      )))

(cl-defun hoarder:register-local (source &optional option)
  (cl-letf* ((path (expand-file-name source))
             (package (hoarder:make-package-local path option)))
    (hoarder:add-to-load-path package)
    (hoarder:add-to-package-list package)
    (hoarder:option-info package)
    (hoarder:message "registered %s locally"
                    (hoarder:package-name package))
    (hoarder:message-register package)
    ))

(cl-defun hoarder:register-theme (source &optional option)
  (cl-letf* ((mod-option (hoarder:register-theme-default-tag option))
             (package (hoarder:make-package source mod-option)))
    (unless (hoarder:installed? package)
      (hoarder:add-to-theme-path package)
      (hoarder:add-to-load-path package)
      (hoarder:add-to-package-list package)
      (hoarder:message "registered %s as theme"
                      (hoarder:package-name package))
      (hoarder:message-register package)
      )))

(cl-defun hoarder:register-theme-local (source &optional option)
  (cl-letf* ((path (expand-file-name source))
             (package (hoarder:make-package-local path option)))
    (hoarder:add-to-theme-path package)
    (hoarder:add-to-package-list package)
    (hoarder:message "registered %s as local theme"
                    (hoarder:package-name package))
    (hoarder:message-register package)
    ))


(cl-defun hoarder:register-theme-default-tag (option)
  (cl-letf ((o (cl-getf option :tag nil)))
    (if o
        (cond ((cl-equalp "theme" o)
               option)
              ((cl-find "theme" o :test #'cl-equalp)
               option)
              (t option))
      (cons :tag (cons "theme" option)))))


(cl-defmethod hoarder:resolve-deps ((package hoarder:<package>))
  (if-let ((deps (hoarder:package-dependency package)))
      (seq-each
       #'hoarder:install-dep
       deps)
    nil))

(cl-defmethod hoarder:install-dep ((dep list))
  (hoarder:register (cl-first dep)
                   (if (cl-rest dep)
                       (cl-second dep)
                     nil)))

(cl-defmethod hoarder:install-dep ((dep string))
  (hoarder:register dep nil))

(provide 'hoarder-register)

;;; register.el ends here
