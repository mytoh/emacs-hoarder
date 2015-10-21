;;; register -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

;;;; register

(cl-defun hoarder:register (source &optional option)
  (pcase source
    ((and (pred file-name-absolute-p)
          (pred file-exists-p))
     (hoarder:handle-register `[:local ,source ,option]))
    (_
     (hoarder:handle-register `[:remote ,source ,option]))))

(cl-defun hoarder:handle-register (variant)
  (pcase variant
    (`[:local ,source ,option]
      (hoarder:register-local
       (hoarder:make-package-local
        (expand-file-name source) option)))
    (`[:remote ,source ,option]
      (hoarder:register-remote
       (hoarder:make-package source option)))))

(cl-defun hoarder:message-register (package)
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

(cl-defun hoarder:register-remote (package)
  (hoarder:resolve-deps package)
  (unless (hoarder:installed? package)
    (hoarder:add-to-load-path package)
    (hoarder:add-to-package-list package)
    (hoarder:option-info package)
    (hoarder:message "registered %s"  (glof:get package :name))
    (hoarder:message-register package)))

(cl-defun hoarder:register-local (package)
  (hoarder:add-to-load-path package)
  (hoarder:add-to-package-list package)
  (hoarder:option-info package)
  (hoarder:message "registered %s locally"
                   (glof:get package :name))
  (hoarder:message-register package))

(cl-defun hoarder:register-theme (source &optional option)
  (cl-letf* ((mod-option (hoarder:register-theme-default-tag option))
             (package (hoarder:make-package source mod-option)))
    (unless (hoarder:installed? package)
      (hoarder:add-to-theme-path package)
      (hoarder:add-to-load-path package)
      (hoarder:add-to-package-list package)
      (hoarder:message "registered %s as theme"
                       (glof:get package :name))
      (hoarder:message-register package)
      )))

(cl-defun hoarder:register-theme-local (source &optional option)
  (cl-letf* ((path (expand-file-name source))
             (package (hoarder:make-package-local path option)))
    (hoarder:add-to-theme-path package)
    (hoarder:add-to-package-list package)
    (hoarder:message "registered %s as local theme"
                     (glof:get package :name))
    (hoarder:message-register package)
    ))


(cl-defun hoarder:register-theme-default-tag (option)
  (cl-letf ((o (glof:get option :tag nil)))
    (pcase o
      (`nil (cons :tag (cons "theme" option)))
      ("theme" option)
      ((guard (cl-find "theme" o :test #'cl-equalp))
       option)
      (_ option))))

(cl-defun hoarder:resolve-deps (package)
  (if-let ((deps (glof:get package :dependency)))
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
