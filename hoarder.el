;;; hoarder.el -*- lexical-binding: t -*-

;;;; requirements
(require 'cl-lib)
(require 'glof)
(require 'seq)

(require 'hoarder-source-github "hoarder/source/github")
(require 'hoarder-source-git "hoarder/source/git")
(require 'hoarder-package "hoarder/package")
(require 'hoarder-util "hoarder/util")

(require 'hoarder-install "hoarder/command/install")
(require 'hoarder-register "hoarder/command/register")
(require 'hoarder-update "hoarder/command/update")
(require 'hoarder-clean "hoarder/command/clean")
(require 'hoarder-uninstall "hoarder/command/uninstall")
(require 'hoarder-search "hoarder/command/search")
(require 'hoarder-check "hoarder/command/check")
(require 'hoarder-reinstall "hoarder/command/reinstall")
(require 'hoarder-fetch "hoarder/command/fetch")

;;;; initialize
(defcustom hoarder-directory
  (hoarder:concat-path user-emacs-directory (file-name-as-directory "hoarder"))
  "default install directory")
(defvar hoarder:*packages* [])

(cl-defun hoarder:initialize (&optional path)
  (setq hoarder:*packages* [])
  (when path
    (setq hoarder-directory path))
  (unless (file-exists-p hoarder-directory)
    (make-directory hoarder-directory 'recursive))
  hoarder-directory)

;;;; font-lock

(defcustom hoarder:font-lock-keywords
  '(hoarder:initialize
    hoarder:turn-on-font-lock
    hoarder:register
    hoarder:register-local
    hoarder:register-theme
    hoarder:register-theme-local
    hoarder:check-packages
    hoarder:fetch
    )
  "hoarder keywords")

(cl-defun hoarder:turn-on-font-lock ()
  (cl-flet ((add-keywords (face-name keyword-rules)
              (cl-letf* ((keyword-list (seq-map (lambda (x)
                                                  (symbol-name (cl-rest x)))
                                                keyword-rules))
                         (keyword-regexp (seq-concatenate 'string "(\\("
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
      hoarder:font-lock-keywords))))

;;; provide
(provide 'hoarder)
