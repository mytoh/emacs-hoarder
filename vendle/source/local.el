;;; local -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(cl-defun vendle:make-package-local (source option)
  (cl-letf ((name (vendle:make-package-name-local source option))
            (load-path (vendle:make-package-load-path-local source option)))
    (make-instance 'vendle:package
                   :type 'local
                   :name name
                   :path source
                   :load-path load-path
                   :url ""
                   :compile nil
                   :build nil
                   :info (cl-getf option :info nil)
                   :origin (cl-getf option :origin source)
                   :tag (cl-getf option :tag nil)
                   :desc (cl-getf option :desc ""))))

(cl-defun vendle:make-package-name-local (source option)
  (if option
      (thread-first option
        (cl-getf :name)
        (if name (file-name-nondirectory source)))
    (file-name-nondirectory source)))

(cl-defun vendle:make-package-load-path-local (source option)
  (if option
      (cl-letf ((path (cl-getf option :load-path)))
        (if path
            (expand-file-name path source)
          source))
    source))

(cl-defun vendle:make-package-url-local (source option)
  (if option
      (if-let ((url (cl-getf option :url)))
          url "")
    ""))

(provide 'vendle-source-local)

;;; local.el ends here
