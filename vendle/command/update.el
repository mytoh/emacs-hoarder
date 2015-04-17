;;; update -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")
(require 'vendle-option "vendle/option")

;;;; update

(cl-defun vendle:update-packages ()
  (when (file-exists-p vendle-directory)
    (vendle:foreach-package-list #'vendle:update-package)))

(cl-defmethod vendle:update-package ((package vendle:<package>))
  (cl-letf ((name (vendle:package-name package))
            (path (vendle:concat-path vendle-directory
                                      (vendle:package-origin package))))
    (when (and (cl-equalp 'git (vendle:package-type package))
               (not (file-symlink-p path)))
      (cl-letf ((reporter (make-progress-reporter
                           (format  "updating package %s..."
                                    (propertize name 'face 'font-lock-type-face)))))
        (cl-letf* ((git-msg (shell-command-to-string
                             (seq-concatenate 'string
                                              "git " " -C " path
                                              " pull ")))
                   (changedp (vendle:git-updatedp git-msg)))
          (when changedp
            (vendle:option-compile package path)
            (vendle:option-build package)))
        (progress-reporter-done reporter)
        (vendle:message "updated %s" path)))))

(cl-defun vendle:git-updatedp (msg)
  (and (not (cl-equalp
             "Already up-to-date.
"
             msg))
       (not (cl-equalp
             "fatal: Not a git repository (or any of the parent directories): .git
"
             msg))))

;; ###autoload
(cl-defun vendle:update ()
  (interactive)
  (vendle:message "package update start")
  (vendle:update-packages)
  (vendle:message "package update finish"))


(provide 'vendle-update)

;;; update.el ends here
