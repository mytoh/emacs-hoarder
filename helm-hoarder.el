;;; helm-hoarder.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'eieio)
(require 'helm)
(require 'hoarder)
(require 'seq)

(cl-defun helm-hoarder-init ()
  (setq helm-hoarder-candidates
        hoarder:*packages*))

(cl-defun helm-hoarder-action-update (_candidate)
  (cl-letf ((pkgs (helm-marked-candidates)))
    (seq-each
     #'helm-hoarder-action-update-1
     pkgs)))

(cl-defmethod helm-hoarder-action-update-1 ((package hoarder:<package>))
  (hoarder:update-package package))

(cl-defun helm-hoarder-action-reinstall (_candidate)
  (cl-letf ((pkgs (helm-marked-candidates)))
    (seq-each
     #'helm-hoarder-action-reinstall-1
     pkgs)))

(cl-defmethod helm-hoarder-action-reinstall-1 ((package hoarder:<package>))
  (hoarder:reinstall-package package))

;; http://rubikitch.com/2014/09/02/helm-quelpa/
(cl-defmethod helm-hoarder-action-magit-log ((package hoarder:<package>))
  (with-helm-default-directory (hoarder:package-path package)
      (magit-log '("HEAD"))))

(cl-defmethod helm-hoarder-action-open-dired ((package hoarder:<package>))
  (dired (hoarder:package-path package)))

(cl-defmethod helm-hoarder-action-find-file ((package hoarder:<package>))
  (helm-find-files-1 (file-name-as-directory (hoarder:package-path package))))

(cl-defmethod helm-hoarder-action-view-readme-or-src ((package hoarder:<package>))
  (cl-loop for file in (list "README.md" "README.org" "README")
     for path = (expand-file-name file (hoarder:package-path package))
     when (file-exists-p path)
     return (view-file path)))

(cl-defun helm-hoarder-source-name/mark (mark name)
  (cond ((window-system)
         (format " %s %s" mark name))
        (t
         name)))

(cl-defun helm-hoarder-transformer-format (candidates)
  (seq-map
   #'helm-hoarder-transformer-format-1
   candidates))

(cl-defmethod helm-hoarder-transformer-format-1 ((package hoarder:<package>))
  (cl-letf ((tag (helm-hoarder-propertize-tag package 'font-lock-doc-face)))
    (cons (format
           "%s%s\t%s"
           (propertize (hoarder:package-name package)
                       'face
                       'font-lock-keyword-face)
           (if tag (seq-concatenate 'string "\t" tag) "")
           (propertize (hoarder:package-origin package)
                       'face
                       'font-lock-variable-name-face))
          package)))

(cl-defmethod helm-hoarder-propertize-tag ((package hoarder:<package>) face)
  (cl-letf ((tag (hoarder:package-tag package)))
    (if tag
        (if (stringp tag)
            (propertize tag 'face face)
          (string-join
           (seq-map (lambda (t) (propertize t 'face face)) tag)
           ","))
      nil)))

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
   (candidate-transformer :initform #'helm-hoarder-transformer-format)))

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
