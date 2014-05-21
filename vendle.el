;;; vendle.el -*- lexical-binding: t -*-

;;;; requirements
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

(require 'vendle-source-github "vendle/source/github")
(require 'vendle-source-git "vendle/source/git")
(require 'vendle-package "vendle/package")
(require 'vendle-util "vendle/util")

(require 'vendle-install "vendle/command/install")
(require 'vendle-register "vendle/command/register")
(require 'vendle-update "vendle/command/update")
(require 'vendle-clean "vendle/command/clean")
(require 'vendle-uninstall "vendle/command/uninstall")
(require 'vendle-search "vendle/command/search")
(require 'vendle-check "vendle/command/check")

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
    (make-directory vendle-directory 'recursive))
  vendle-directory)


;;;###autoload
(cl-defun vendle-reinstall ()
  (interactive)
  (vendle:message "package reinstall start")
  (vendle:reinstall-package)
  (vendle:message "package reinstall finish"))

;;;; font-lock

(defcustom vendle:font-lock-keywords
  '(vendle:initialize
    vendle:turn-on-font-lock
    vendle:register
    vendle:register-local
    vendle:register-theme
    vendle:register-theme-local
    vendle:check-packages)
  "vendle keywords")

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
     (cl-mapcar
      (lambda (key)
        (cons 1 key))
      vendle:font-lock-keywords))))

;;; provide
(provide 'vendle)
