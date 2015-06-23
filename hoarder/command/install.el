;;; install -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")

;;;; install

(cl-defmethod hoarder:install-package ((package hoarder:<package>))
  (unless (or (cl-equalp 'local (hoarder:package-type package))
              (file-exists-p (hoarder:package-path package)))
    (pcase (hoarder:package-type package)
      ('git (hoarder:install-package-git package)))))

(cl-defmethod hoarder:install-package-git ((package hoarder:<package>))
  (hoarder:message "installing package %s" (hoarder:package-name package))
  (shell-command (seq-concatenate 'string  "git clone --quiet "
                                  (if (hoarder:package-recursive package)
                                      " --recursive "
                                    "")
                                  (if (hoarder:package-branch package)
                                      (seq-concatenate 'string
                                                       " --branch "
                                                       (hoarder:package-branch package)
                                                       " ")
                                    "")
                                  (hoarder:package-url package)
                                  " "
                                  (hoarder:concat-path hoarder-directory
                                                      (hoarder:package-origin package)))
                 hoarder-directory)
  (hoarder:message "compiling %s" (hoarder:package-name package))
  (hoarder:option-compile package (hoarder:package-path package))
  (hoarder:option-build package)
  (hoarder:option-info package))

(provide 'hoarder-install)

;;; install.el ends here
