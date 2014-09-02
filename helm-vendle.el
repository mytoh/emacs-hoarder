;;; helm-vendle.el -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))
(require 'helm)
(require 'vendle)

(cl-defun helm-vendle-init ()
  (setq helm-vendle-candidates
        *vendle-package-list*))

(cl-defun helm-vendle-action-update (candidate)
  (vendle:update-package candidate))

(cl-defun helm-vendle-action-reinstall (candidate)
  (vendle:reinstall-package candidate))

;; http://rubikitch.com/2014/09/02/helm-quelpa/
(cl-defun helm-vendle-action-magit-log (package)
  (cl-letf ((default-directory (vendle:package-path package)))
    (magit-status ".")
    (magit-log)))

(cl-defun helm-vendle-action-open-dired (package)
  (dired (vendle:package-path package)))

(defun helm-vendle-action-find-file (package)
  (helm-find-files-1 (vendle:package-path package)))

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
  (cl-mapcar
   (lambda (package)
     (cons (format
            "%s"
            (propertize (vendle:package-name package)
                        'face
                        'font-lock-keyword-face))
           package))
   candidates))

(defvar helm-source-vendle-list
  `((name . ,(helm-vendle-source-name/mark "📦" "Packages"))
    (init . helm-vendle-init)
    (candidates . helm-vendle-candidates)
    (action . (("Update" . helm-vendle-action-update)
               ("Reinstall" . helm-vendle-action-reinstall)
               ("Magit log" . helm-vendle-action-magit-log)
               ("View README or source" . helm-vendle-action-view-readme-or-src)
               ("Open directory" . helm-vendle-action-open-dired)
               ("Find file" . helm-vendle-action-find-file)))
    (candidate-transformer
     helm-vendle-transformer-format)))

;;;###autoload
(cl-defun helm-vendle ()
  "Preconfigured `helm' for vendle package list source. "
  (interactive)
  (helm :sources '(helm-source-vendle-list)
        :buffer "*helm vendle*"))


(provide 'helm-vendle)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
