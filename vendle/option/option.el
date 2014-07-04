;;; option -*- lexical-binding: t -*-

;;; Code:

(defmethod vendle:resolve-deps ((package vendle:package))
  (cl-letf ((deps (vendle:package-deps package)))
    (if deps
        (cl-mapc
         'vendle:install-dep
         deps)
      nil)))

(cl-defun vendle:install-dep (information)
  (cl-typecase information
    (list
     (vendle:register (car information)
                      (cdr information)))
    (string
     (vendle:register information nil))))


;;; option.el ends here