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
               ("Reinstall" . helm-vendle-action-reinstall)))
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
