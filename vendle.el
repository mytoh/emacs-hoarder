;;; vendle.el -*- lexical-binding: t -*-

;;; requirements
(eval-when-compile
  (require 'cl-lib))

(require 'vendle-site-github "vendle/site/github")
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

(defmethod vendle:compile ((package vendle:package) path)
  (if (vendle:package-compile package)
      (byte-recompile-directory path 0)))

(cl-defun vendle:message (fmt &rest text)
  (apply 'message (format "[%s] %s"
                          (propertize "vendle"
                                      'face '(:foreground "#539b8f"))
                          fmt)
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
(defcustom vendle-directory
  (vendle:concat-path user-emacs-directory (file-name-as-directory "vendle"))
  "default install directory")
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

(defmethod vendle:update-package ((_package vendle:package))
  (cl-letf ((path (vendle:concat-path vendle-directory (vendle:package-name _package))))
    (when (and (cl-equalp 'git (vendle:package-type _package))
               (not (file-symlink-p path)))
      (progn
        (cd-absolute path)
        (vendle:message "updating vendle package %s.." path)
        (shell-command "git pull")
        (cd-absolute user-emacs-directory)
        (vendle:compile _package path)
        (vendle:message "updating vendle package %s.. done" path)))))

;;;; install

(defmethod vendle:install-package ((package vendle:package))
  (unless (or (cl-equalp 'local (vendle:package-type package))
              (file-exists-p (vendle:package-path package)))
    (cond ((cl-equalp 'git (vendle:package-type package))
           (vendle:install-package-git package)))))

(defmethod vendle:install-package-git ((_package vendle:package))
  (vendle:message "installing package %s" (vendle:package-name _package))
  (shell-command (concat  "git clone " (vendle:package-url _package)
                          " "
                          (vendle:concat-path vendle-directory (vendle:package-name _package)))
                 vendle-directory)
  (vendle:message "compiling %s" (vendle:package-name _package))
  (vendle:compile _package (vendle:package-path _package)))

;;;; check
(cl-defun vendle:check-packages ()
  (cl-mapc
   (lambda (package)
     (vendle:install-package package))
   *vendle-package-list*))

;;;; register

(cl-defun vendle:register (_source &optional _info)
  (cl-letf* ((package (vendle:make-package _source _info)))
    (vendle:add-to-load-path (vendle:package-path package))
    (vendle:add-to-package-list package)
    (vendle:message "registered %s"    (vendle:package-name package))))

(cl-defun vendle:register-local (source &optional info)
  (cl-letf* ((path (expand-file-name source))
             (package (vendle:make-package-local path info)))
    (vendle:add-to-load-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)
    (vendle:message "registered %s locally"
                    (vendle:package-name package))))

(cl-defun vendle:register-theme (source &optional info)
  (cl-letf* ((package (vendle:make-package source info)))
    (vendle:add-to-theme-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)
    (vendle:message "registered %s as theme"
                    (vendle:package-name package))))

(cl-defun vendle:register-theme-local (_source &optional _info)
  (cl-letf* ((path (expand-file-name _source))
             (package (vendle:make-package-local path _info)))
    (vendle:add-to-theme-path
     (vendle:package-path package))
    (vendle:add-to-package-list package)
    (vendle:message "registered %s as local theme"
                    (vendle:package-name package))))

;;;; clean
(cl-defun vendle:clean-packages ()
  (cl-letf ((paths (cl-remove-if
                    (lambda (dir)
                      (vendle:search-registered dir 'path))
                    (directory-files vendle-directory 'absolute (rx (not (any ".")))))))
    (if paths
        (cl-mapc
         (lambda (p)
           (vendle:message "clean %s" p)
           (delete-directory p t))
         paths))))

;;;; commands

;;;###autoload
(cl-defun vendle-check ()
  "Install packages using `vendle:install-packages'"
  (interactive)
  (vendle:message "package check started")
  (vendle:check-packages)
  (vendle:message "package check finished"))

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
