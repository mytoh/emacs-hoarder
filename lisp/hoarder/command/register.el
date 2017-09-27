;;; register -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'colle)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-source-hg "hoarder/source/hg")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

;;;; register

(cl-defun hoarder:register (source &optional option)
  (declare (indent 1))
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

(cl-defun hoarder:register-remote (p)
  (cl-letf ((package (hoarder:append-tag p "emacs")))
    (hoarder:resolve-deps package)
    (unless (hoarder:installed? package)
      (hoarder:add-to-load-path package)
      (hoarder:add-to-package-list package)
      (hoarder:option-info package)
      (unless noninteractive
        (hoarder:message "registered %s"  (glof:get package :name)))
      (hoarder:message-register package))))

(cl-defun hoarder:register-local (package)
  (declare (indent 1))
  (hoarder:add-to-load-path package)
  (hoarder:add-to-package-list package)
  (hoarder:option-info package)
  (unless noninteractive
    (hoarder:message "registered %s locally"
               (glof:get package :name)))
  (hoarder:message-register package))

(cl-defun hoarder:register-theme (source &optional option)
  (declare (indent 1))
  (cl-letf* ((moded (thread-first option
                      hoarder:register-theme-default-tag
                      (hoarder:append-tag "emacs")))
             (package (hoarder:make-package source moded)))
    (unless (hoarder:installed? package)
      (hoarder:add-to-theme-path package)
      (hoarder:add-to-load-path package)
      (hoarder:add-to-package-list package)
      (unless noninteractive
        (hoarder:message "registered %s as theme"
                   (glof:get package :name)))
      (hoarder:message-register package))))

(cl-defun hoarder:register-theme-local (source &optional option)
  (declare (indent 1))
  (cl-letf* ((path (expand-file-name source))
             (package (hoarder:make-package-local path option)))
    (hoarder:add-to-theme-path package)
    (hoarder:add-to-package-list package)
    (unless noninteractive
      (hoarder:message "registered %s as local theme"
                 (glof:get package :name)))
    (hoarder:message-register package)
    ))

(cl-defun hoarder:append-tag (option tag)
  (glof:update option
             :tags
             (lambda (tags)
               (pcase tags
                 (`()
                  `[,tag])
                 ((pred (seq-find (lambda (tg) (cl-equalp tg tag))))
                  tags)
                 (_ (colle:conj tags tag))))))

(cl-defun hoarder:register-theme-default-tag (option)
  (cl-letf ((o (glof:get option :tags nil)))
    (pcase o
      (`nil (glof:assoc option
                      :tags ["theme"]))
      ("theme" option)
      ((pred stringp)
       (glof:assoc option
                 :tags
                 (seq-concatenate 'vector
                                  `[,o]
                                  ["theme"])))
      ((pred (seq-find (lambda (tag) (equal tag "theme"))))
       option)
      (_
       (glof:update option :tags
                  (lambda (tags)
                    (seq-concatenate 'vector
                                     tags
                                     ["theme"])))))))

(cl-defun hoarder:resolve-deps (package)
  (cl-letf ((deps (glof:get package :dependencies)))
    (unless (seq-empty-p deps)
      (seq-each
       #'hoarder:install-dep
       deps))
    nil))

(cl-defun hoarder:install-dep (dep)
  (cl-etypecase dep
    (list
     (hoarder:register (cl-first dep)
       (if (cl-rest dep)
           (cl-second dep)
         nil)))
    (string
     (hoarder:register dep nil))))


(provide 'hoarder-register)

;;; register.el ends here
