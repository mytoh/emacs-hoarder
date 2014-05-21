;;; update -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

;;;; update

(cl-defun vendle:update-packages ()
  (when (file-exists-p vendle-directory)
    (vendle:map-package-list 'vendle:update-package)))

(defmethod vendle:update-package ((_package vendle:package))
  (cl-letf ((name (vendle:package-name _package))
            (path (vendle:concat-path vendle-directory (vendle:package-name _package))))
    (when (and (cl-equalp 'git (vendle:package-type _package))
               (not (file-symlink-p path)))
      (cl-locally
          (vendle:message "updating package %s..."
                          (propertize name 'face 'font-lock-type-face))
        (shell-command (concat
                        "git pull --quiet"
                        " -C " path))
        (cd-absolute user-emacs-directory)
        (vendle:compile _package path)
        (vendle:message "updating package %s... done" path)))))

;;;###autoload
(cl-defun vendle-update ()
  (interactive)
  (vendle:message "package update start")
  (vendle:update-packages)
  (vendle:message "package update finish"))


(provide 'vendle-update)

;;; update.el ends here
