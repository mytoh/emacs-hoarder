;;; helm-vendle.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))
(require 'eieio)
(require 'helm)
(require 'vendle)
(require 'seq)

(cl-defun helm-vendle-init ()
  (setq helm-vendle-candidates
        *vendle-package-list*))

(cl-defun helm-vendle-action-update (candidate)
  (vendle:update-package candidate))

(cl-defun helm-vendle-action-reinstall (candidate)
  (vendle:reinstall-package candidate))

;; http://rubikitch.com/2014/09/02/helm-quelpa/
(cl-defun helm-vendle-action-magit-log (package)
  (cl-letf ((default-directory (file-name-as-directory (vendle:package-path package))))
    (magit-status ".")
    (magit-log)))

(cl-defun helm-vendle-action-open-dired (package)
  (dired (vendle:package-path package)))

(defun helm-vendle-action-find-file (package)
  (helm-find-files-1 (file-name-as-directory (vendle:package-path package))))

(cl-defun helm-vendle-action-view-readme-or-src (package)
  (cl-loop for file in (list "README.md" "README.org" "README")
     for path = (expand-file-name file (vendle:package-path package))
     when (file-exists-p path)
     return (view-file path)))

(cl-defun helm-vendle-source-name/mark (mark name)
  (cond ((window-system)
         (format " %s %s" mark name))
        (t
         name)))

(cl-defun helm-vendle-transformer-format (candidates)
  (seq-map
   (lambda (package)
     (cl-letf ((tag (helm-vendle-format-tag package 'font-lock-doc-face)))
       (cons (format
              "%s%s\t%s"
              (propertize (vendle:package-name package)
                          'face
                          'font-lock-keyword-face)
              (if tag (concat "\t" tag) "")
              (propertize (vendle:package-origin package)
                          'face
                          'font-lock-variable-name-face))
             package)))
   candidates))

(cl-defun helm-vendle-format-tag (package face)
  (cl-letf ((tag (vendle:package-tag package)))
    (if tag
        (if (stringp tag)
            (propertize tag 'face face)
          (string-join
           (seq-map (lambda (t) (propertize t 'face face)) tag)
           ","))
      nil)))

(cl-defun helm-vendle-make-source-name ()
  (helm-vendle-source-name/mark "📦" "Packages"))

(defclass helm-source-vendle-package-list (helm-source-sync)
  ((name :initform helm-vendle-make-source-name)
   (init :initform  helm-vendle-init)
   (candidates :initform helm-vendle-candidates)
   (action :initform
           (helm-make-actions
            "Update"  'helm-vendle-action-update
            "Reinstall"  'helm-vendle-action-reinstall
            "Magit log"  'helm-vendle-action-magit-log
            "View README or source"  'helm-vendle-action-view-readme-or-src
            "Open directory" 'helm-vendle-action-open-dired
            "Find file"  'helm-vendle-action-find-file))
   (candidate-transformer :initform helm-vendle-transformer-format)))

(defvar helm-source-vendle-list
  (helm-make-source "Vendle Package List" 'helm-source-vendle-package-list))

;;;###autoload
(cl-defun helm-vendle ()
  "Preconfigured `helm' for vendle package list source. "
  (interactive)
  (helm :sources (list 'helm-source-vendle-list)
        :buffer "*helm vendle*"))


(provide 'helm-vendle)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
