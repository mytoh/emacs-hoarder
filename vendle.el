;;; vendle.el -*- lexical-binding: t -*-

;;;; requirements
(require 'cl-lib)
(require 'eieio)
(require 'seq)

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
(require 'vendle-reinstall "vendle/command/reinstall")
(require 'vendle-fetch "vendle/command/fetch")

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

;;;; font-lock

(defcustom vendle:font-lock-keywords
  '(vendle:initialize
    vendle:turn-on-font-lock
    vendle:register
    vendle:register-local
    vendle:register-theme
    vendle:register-theme-local
    vendle:check-packages
    vendle:fetch
    )
  "vendle keywords")

(cl-defun vendle:turn-on-font-lock ()
  (cl-flet ((add-keywords (face-name keyword-rules)
              (cl-letf* ((keyword-list (seq-map (lambda (x)
                                                  (symbol-name (cl-rest x)))
                                                keyword-rules))
                         (keyword-regexp (concat "(\\("
                                                 (regexp-opt keyword-list)
                                                 "\\)\\>")))
                (font-lock-add-keywords  'emacs-lisp-mode
                                         `((,keyword-regexp 1 ',face-name))))
              (seq-each (lambda (x)
                          (put (cl-rest x)
                               'scheme-indent-function
                               (cl-first x)))
                        keyword-rules)))

    (add-keywords
     'font-lock-builtin-face
     (seq-map
      (lambda (key)
        (cons 1 key))
      vendle:font-lock-keywords))))

;;; provide
(provide 'vendle)
