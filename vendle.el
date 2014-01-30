;;; vendle.el -*- lexical-binding: t -*-

;;; requirements
(eval-when-compile
  (require 'cl-lib))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")

;;;; internal functions

(cl-defun vendle:directory-git-p (p)
  (if (file-directory-p (expand-file-name ".git" p))
      t nil))

(cl-defun vendle:add-to-load-path (path)
  (add-to-list 'load-path path))

(cl-defun vendle:add-to-theme-path (path)
  (add-to-list 'custom-theme-load-path path))

(cl-defun vendle:add-to-package-list (package)
  (add-to-list '*vendle-package-list* package))

;;;; utilily functions

(cl-defun vendle:concat-path (&rest parts)
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

(cl-defun vendle:compile (package path)
  (if (vendle:package-compile package)
      (byte-recompile-directory path 0)))

(cl-defun vendle:message (fmt &rest text)
  (apply 'message (cl-concatenate 'string "vendle: " fmt)
         text))

;;;; search
(cl-defun vendle:search-registered (_key _term)
  (cl-remove-if-not
   (lambda (p)
     (cl-case _term
       (name
        (cl-equalp _key (vendle:package-name p)))
       (type
        (cl-equalp _key (vendle:package-type p)))
       (path
        (cl-equalp _key (vendle:package-path p)))))
   *vendle-package-list*))

;;;; initialize

(defvar vendle-directory (expand-file-name (file-name-as-directory "vendle") user-emacs-directory))
(defvar *vendle-package-list* '())

(cl-defun vendle:initialize (&optional path)
  (setq *vendle-package-list* nil)
  (when path
    (setq vendle-directory path))
  (unless (file-exists-p vendle-directory)
    (make-directory vendle-directory))
  vendle-directory)

;;;; update

(cl-defun vendle:update-packages ()
  (when (file-exists-p vendle-directory)
    (cl-mapc
     'vendle:update-package
     *vendle-package-list*)))

(cl-defun vendle:update-package (package)
  (cl-letf ((path (vendle:concat-path vendle-directory (vendle:package-name package))))
    (when (and (or (cl-equalp 'git (vendle:package-type package))
                   (cl-equalp 'github (vendle:package-type package)))
               (not (file-symlink-p path)))
      (progn
        (cd-absolute path)
        (vendle:message "updating vendle package %s.." path)
        (shell-command "git pull")
        (cd-absolute user-emacs-directory)
        (vendle:compile package path)
        (vendle:message "updating vendle package %s.. done" path)))))

;;;; install

(cl-defun vendle:install-package (package)
  (unless (or (cl-equalp 'local (vendle:package-type package))
              (file-exists-p (vendle:package-path package)))
    (cond ((cl-equalp 'github (vendle:package-type package))
           (vendle:install-package-github package)))))

(cl-defun vendle:install-package-github (package)
  (message "vendle: installing package %s" (vendle:package-name package))
  (shell-command (concat  "git clone " (vendle:package-url package) " "
                          (vendle:concat-path vendle-directory (vendle:package-name package)))
                 vendle-directory)
  (vendle:compile package (vendle:package-path package)))

;;;; check
(cl-defun vendle:check-packages ()
  (cl-mapc
   (lambda (package)
     (vendle:install-package package))
   *vendle-package-list*))

;;;; register

(cl-defun vendle:register (source &optional info)
  (cl-letf* ((package (vendle:make-package source info)))
    (vendle:add-to-load-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)))

(cl-defun vendle:register-local (source &optional info)
  (cl-letf* ((path (expand-file-name source))
             (package (vendle:make-package-local path info)))
    (vendle:add-to-load-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)))

(cl-defun vendle:register-theme (source &optional info)
  (cl-letf* ((package (vendle:make-package source info)))
    (vendle:add-to-theme-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)))

(cl-defun vendle:register-theme-local (source &optional info)
  (cl-letf* ((path (expand-file-name source))
             (package (vendle:make-package-local path info)))
    (vendle:add-to-theme-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)))

;;;; clean
(cl-defun vendle:clean-packages ()
  (cl-letf ((paths (cl-remove-if
                    (lambda (dir)
                      (vendle:search-registered dir 'path))
                    (directory-files vendle-directory 'absolute (rx (not (any ".")))))))
    (if paths
        (cl-mapc
         (lambda (p)
           (message "vendle: clean %s" p)
           (delete-directory p t))
         paths))))

;;;; commands

;;;###autoload
(cl-defun vendle-check ()
  "Install packages using `vendle:install-packages'"
  (interactive)
  (vendle:check-packages))

;;;###autoload
(cl-defun vendle-update ()
  (interactive)
  (vendle:update-packages)
  (vendle:message "package update finished."))

;;;###autoload
(cl-defun vendle-clean ()
  (interactive)
  (vendle:clean-packages))

;;;; font-lock
(cl-defun vendle:turn-on-font-lock ()
  (cl-flet ((add-keywords (face-name keyword-rules)
                          (cl-letf* ((keyword-list (cl-mapcar (lambda (x)
                                                                (symbol-name (cdr x)))
                                                              keyword-rules))
                                     (keyword-regexp (concat "(\\("
                                                             (regexp-opt keyword-list)
                                                             "\\)\\>")))
                            (font-lock-add-keywords  'emacs-lisp-mode
                                                     `((,keyword-regexp 1 ',face-name))))
                          (cl-mapc (lambda (x)
                                     (put (cdr x)
                                          'scheme-indent-function
                                          (car x)))
                                   keyword-rules)))

    (add-keywords
     'font-lock-builtin-face
     '((1 . vendle:initialize)
       (1 . vendle:turn-on-font-lock)
       (1 . vendle:register)
       (1 . vendle:register-local)
       (1 . vendle:register-theme)
       (1 . vendle:register-theme-local)))))

;;; provide
(provide 'vendle)
