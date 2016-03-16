;;; install -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'glof)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")

;;;; install

(cl-defun hoarder:install-package (package)
  (unless (or (cl-equalp :local (glof:get package :type))
             (and (glof:get package :path)
                (file-exists-p (glof:get package :path))))
    (pcase (glof:get package :type)
      (:git (hoarder:install-package-git package)))))

(cl-defun hoarder:install-package-git (package)
  (hoarder:message "installing package %s" (glof:get package :name))
  (cl-letf ((process-environment process-environment))
    (setenv "GIT_TERMINAL_PROMPT" "0")
    (shell-command (seq-concatenate 'string  "git --no-pager clone --quiet "
                                    (if (glof:get package :recursive)
                                        " --recursive "
                                      "")
                                    (if (glof:get package :branch)
                                        (seq-concatenate 'string
                                                         " --branch "
                                                         (glof:get package :branch)
                                                         " ")
                                      "")
                                    (glof:get package :url)
                                    " "
                                    (hoarder:concat-path hoarder-directory
                                                   (glof:get package :path)))
                   hoarder-directory))
  (hoarder:message "compiling %s" (glof:get package :name))
  (hoarder:option-compile package (glof:get package :path))
  (hoarder:option-build package)
  (hoarder:option-info package))

(provide 'hoarder-install)

;;; install.el ends here
