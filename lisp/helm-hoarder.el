;;; helm-hoarder.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'glof)
(require 'helm)
(require 'hoarder)
(require 'seq)

(cl-defun helm-hoarder-init ()
  (setq helm-hoarder-candidates
        (seq-into hoarder:*packages*
                  'list)))

(cl-defun helm-hoarder-action-update (_candidate)
  (cl-letf ((pkgs (helm-marked-candidates)))
    (seq-each
     #'helm-hoarder-action-update-1
     pkgs)))

(cl-defun helm-hoarder-action-update-1 (package)
  (hoarder:update-package package))

(cl-defun helm-hoarder-action-reinstall (_candidate)
  (cl-letf ((pkgs (helm-marked-candidates)))
    (seq-each
     #'helm-hoarder-action-reinstall-1
     pkgs)))

(cl-defun helm-hoarder-action-reinstall-1 (package)
  (hoarder:reinstall-package package))

;; http://rubikitch.com/2014/09/02/helm-quelpa/
(cl-defun helm-hoarder-action-magit-log (package)
  (with-helm-default-directory (glof:get package :path)
      (magit-log '("HEAD"))))

(cl-defun helm-hoarder-action-open-dired (package)
  (dired (glof:get package :path)))

(cl-defun helm-hoarder-action-find-file (package)
  (helm-find-files-1 (file-name-as-directory (glof:get package :path))))

(cl-defun helm-hoarder-action-view-readme-or-src (package)
  (cl-loop for file in (list "README.md" "README.org" "README")
           for path = (expand-file-name file (glof:get package :path))
           when (file-exists-p path)
           return (view-file path)))

(cl-defun helm-hoarder-source-name/mark (mark name)
  (cond ((window-system)
         (format " %s %s" mark name))
        (t
         name)))

(cl-defun helm-hoarder-transformer-remove-local-packages (candidates)
  (seq-filter
   (lambda (c)
     (not (cl-equalp :local (glof:get c :type))))
   candidates))

(cl-defun helm-hoarder-transformer-format (candidates)
  (seq-map
   #'helm-hoarder-transformer-format-1
   candidates))

(cl-defun helm-hoarder-transformer-format-1 (package)
  (cl-letf ((tag (helm-hoarder-propertize-tag package 'font-lock-doc-face)))
    (cons (format
           "%s%s\t%s"
           (propertize (glof:get package :name)
                       'face
                       'font-lock-keyword-face)
           (if tag (seq-concatenate 'string "\t" tag) "")
           (if (glof:get package :origin "")
               (propertize (glof:get package :origin "")
                           'face
                           'font-lock-variable-name-face)
             ""))
          package)))

(cl-defun helm-hoarder-propertize-tag (package face)
  (cl-letf ((tag (glof:get package :tags)))
    (pcase tag
      (`nil nil)
      ((pred stringp)
       (propertize tag 'face face))
      ((pred seqp)
       (string-join
        (seq-into
         (seq-map
          (lambda (tg) (propertize tg 'face face))
          tag) 'list)
        ",")))))

(cl-defun helm-hoarder-make-source-name ()
  (helm-hoarder-source-name/mark "📦" "Packages"))

(defclass helm-source-hoarder-package-list (helm-source-sync)
  ((init :initform #'helm-hoarder-init)
   (candidates :initform 'helm-hoarder-candidates)
   (action :initform
     (helm-make-actions
      "Update"  #'helm-hoarder-action-update
      "Reinstall"  #'helm-hoarder-action-reinstall
      "Magit log"  #'helm-hoarder-action-magit-log
      "View README or source"  #'helm-hoarder-action-view-readme-or-src
      "Open directory" #'helm-hoarder-action-open-dired
      "Find file"  #'helm-hoarder-action-find-file))
   (candidate-transformer :initform '(helm-hoarder-transformer-remove-local-packages
                                      helm-hoarder-transformer-format))))

(defvar helm-source-hoarder-list
  (helm-make-source (helm-hoarder-make-source-name)
      'helm-source-hoarder-package-list))

;;;###autoload
(cl-defun helm-hoarder ()
  "Preconfigured `helm' for hoarder package list source. "
  (interactive)
  (helm :sources (list 'helm-source-hoarder-list)
        :buffer "*helm hoarder*"))


(provide 'helm-hoarder)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
