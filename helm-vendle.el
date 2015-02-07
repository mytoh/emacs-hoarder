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

(cl-defun helm-vendle-action-update (_candidate)
  (cl-letf ((pkgs (helm-marked-candidates)))
    (seq-each
     #'helm-vendle-action-update-1
     pkgs)))

(cl-defmethod helm-vendle-action-update-1 ((package vendle:package))
  (vendle:update-package package))

(cl-defun helm-vendle-action-reinstall (_candidate)
  (cl-letf ((pkgs (helm-marked-candidates)))
    (seq-each
     #'helm-vendle-action-reinstall-1
     pkgs)))

(cl-defmethod helm-vendle-action-reinstall-1 ((package vendle:package))
  (vendle:reinstall-package package))

;; http://rubikitch.com/2014/09/02/helm-quelpa/
(cl-defmethod helm-vendle-action-magit-log ((package vendle:package))
  (with-helm-default-directory (vendle:package-path package)
      (magit-status ".")
    (magit-log)))

(cl-defmethod helm-vendle-action-open-dired ((package vendle:package))
  (dired (vendle:package-path package)))

(cl-defmethod helm-vendle-action-find-file ((package vendle:package))
  (helm-find-files-1 (file-name-as-directory (vendle:package-path package))))

(cl-defmethod helm-vendle-action-view-readme-or-src ((package vendle:package))
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
   #'helm-vendle-transformer-format-1
   candidates))

(cl-defmethod helm-vendle-transformer-format-1 ((package vendle:package))
  (cl-letf ((tag (helm-vendle-propertize-tag package 'font-lock-doc-face)))
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

(cl-defmethod helm-vendle-propertize-tag ((package vendle:package) face)
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
  ((init :initform #'helm-vendle-init)
   (candidates :initform 'helm-vendle-candidates)
   (action :initform
           (helm-make-actions
            "Update"  #'helm-vendle-action-update
            "Reinstall"  #'helm-vendle-action-reinstall
            "Magit log"  #'helm-vendle-action-magit-log
            "View README or source"  #'helm-vendle-action-view-readme-or-src
            "Open directory" #'helm-vendle-action-open-dired
            "Find file"  #'helm-vendle-action-find-file))
   (candidate-transformer :initform #'helm-vendle-transformer-format)))

(defvar helm-source-vendle-list
  (helm-make-source (helm-vendle-make-source-name)
      'helm-source-vendle-package-list))

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
