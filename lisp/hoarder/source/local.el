;;; local -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'colle)

(cl-defun hoarder:make-package-local (source option)
  (cl-letf ((name (hoarder:make-package-name-local source option))
            (lpath (hoarder:make-package-load-path-local source option)))
    (glof:plist
     :build nil
     :compile nil
     :description (glof:get option :description "")
     :download nil
     :info (glof:get option :info nil)
     :load-path lpath
     :name name
     :origin source
     :path source
     :tags (glof:get option :tags nil)
     :type :local
     :url "")))

(cl-defun hoarder:make-package-name-local (source option)
  (pcase option
    (`nil (file-name-nondirectory source))
    (_ (pcase (glof:get option :name)
         (`nil (file-name-nondirectory source))
         (name name)))))

(cl-defun hoarder:make-package-load-path-local (source option)
  (if option
      (cl-letf ((path (glof:get option :load-path nil)))
        (pcase path
          ((pred vectorp)
           (colle:map
            (lambda (p)
              (expand-file-name p source))
            path))
          ((pred seq-empty-p)
           source)))
    source))

(cl-defun hoarder:make-package-url-local (source option)
  (pcase option
    (`nil "")
    (_ (if-let* ((url (glof:get option :url)))
           url
         ""))))

(provide 'hoarder-source-local)

;;; local.el ends here
