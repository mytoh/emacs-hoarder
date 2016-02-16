;;; update -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'glof)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")
(require 'hoarder-option "hoarder/option")

;;;; update

(cl-defun hoarder:update-all-packages ()
  (when (file-exists-p hoarder-directory)
    (hoarder:foreach-package-list #'hoarder:update-package)))

(cl-defun hoarder:update-package (package)
  (cl-letf ((name (glof:get package :name))
            (path (hoarder:concat-path hoarder-directory
                                 (glof:get package :origin))))
    (when (and (cl-equalp :git (glof:get package :type))
             (not (file-symlink-p path)))
      (cl-letf ((reporter (make-progress-reporter
                           (format  "updating package %s..."
                                    (propertize name 'face 'font-lock-type-face)))))
        (cl-letf* ((git-msg (shell-command-to-string
                             (seq-concatenate 'string
                                              "git " " -C " path
                                              " pull " " --rebase " " --ff-only ")))
                   (already-updatedp (hoarder:git-already-updatedp git-msg)))
          (unless already-updatedp
            (hoarder:option-compile package path)
            (hoarder:option-build package)))
        (progress-reporter-done reporter)
        (hoarder:message "updated %s" path)))))

(cl-defun hoarder:git-already-updatedp (msg)
  (and (not (cl-equalp
         "fatal: Not a git repository (or any of the parent directories): .git
"
         msg))
     (string-match-p
      "Already\sup-to-date\." 
      msg)))

;; ###autoload
(cl-defun hoarder:update ()
  (interactive)
  (hoarder:message "package update start")
  (hoarder:update-all-packages)
  (hoarder:message "package update finish"))


(provide 'hoarder-update)

;;; update.el ends here
