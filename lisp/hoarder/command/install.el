;;; install -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'glof)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")

;;;; install

(cl-defun hoarder:install-package (package)
  (when (hoarder:should-install-package package)
    (pcase (glof:get package :type)
      (:git (hoarder:install-package-git package))
      (:hg (hoarder:install-package-hg package)))))

(cl-defun hoarder:should-install-package (package)
  (cl-labels ((localp (p)
                      (cl-equalp :local (glof:get p :type)))
              (remotep (p)
                       (not (localp p)))
              (downloadp (p)
                         (glof:get p :download nil))
              (installedp (p)
                          (and (glof:get p :path)
                             (file-exists-p (glof:get p :path)))))
    (and (remotep package)
       (downloadp package)     
       (not (installedp package)))))

(cl-defun hoarder:install-package-git (package)
  (hoarder:message "installing package %s" (glof:get package :name))
  (cl-letf ((process-environment process-environment))
    (setenv "GIT_TERMINAL_PROMPT" "0")
    (cl-letf ((proc
               (make-process :name (format "hoarder-install-%s" (glof:get package :origin))
                             :buffer (get-buffer-create (format "hoarder-install-%s" (glof:get package :origin)))
                             :sentinel
                             (lambda (p s)
                               (pcase s
                                 ("finished\n"
                                  (kill-buffer (process-buffer p)))
                                 (_ nil)))
                             :command
                             (seq-remove
                              #'null
                              `("git" "--no-pager" "clone" "--quiet"
                                ,@(pcase (glof:get package :recursive)
                                    (`nil nil)
                                    (`t
                                     (list "--recursive"
                                           "--shallow-submodules")))
                                ,@(pcase (glof:get package :branch)
                                    (`nil nil)
                                    (branch
                                     (list "--branch" branch)))
                                ,@(pcase (glof:get package :depth)
                                    (`nil nil)
                                    (depth
                                     (list (concat "--depth="
                                                   (number-to-string depth)))))
                                "--"
                                ,(glof:get package :url)
                                ,(hoarder:concat-path hoarder-directory
                                                (glof:get package :path)))))))
      ;; (hoarder:message "COMMAND: %s" (pp-to-string (process-command proc)))
      (accept-process-output proc)))
  (hoarder:message "compiling %s" (glof:get package :name))
  (hoarder:option-compile package (glof:get package :path))
  (hoarder:option-build package)
  (hoarder:option-info package))

(cl-defun hoarder:install-package-hg (package)
  (hoarder:message "installing package %s" (glof:get package :name))
  (cl-letf ((process-environment process-environment))
    (setenv "GIT_TERMINAL_PROMPT" "0")
    (shell-command (seq-concatenate 'string  "hg clone "
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
