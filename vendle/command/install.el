;;; install -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")

;;;; install

(defmethod vendle:install-package ((package vendle:package))
  (unless (or (cl-equalp 'local (vendle:package-type package))
              (file-exists-p (vendle:package-path package)))
    (cond ((cl-equalp 'git (vendle:package-type package))
           (vendle:install-package-git package)))))

(defmethod vendle:install-package-git ((package vendle:package))
  (vendle:message "installing package %s" (vendle:package-name package))
  (shell-command (concat  "git clone --quiet " (vendle:package-url package)
                          " "
                          (vendle:concat-path vendle-directory (vendle:package-name package)))
                 vendle-directory)
  (vendle:message "compiling %s" (vendle:package-name package))
  (vendle:compile package (vendle:package-path package)))

(provide 'vendle-install)

;;; install.el ends here
