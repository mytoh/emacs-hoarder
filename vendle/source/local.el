;;; local -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(cl-defun vendle:make-package-local (source info)
  (cl-letf ((name (vendle:make-package-name-local source info))
            (load-path (vendle:make-package-load-path-local source info)))
    (vendle:package name
                    :type 'local
                    :name name
                    :path source
                    :load-path load-path
                    :url ""
                    :compile nil
                    :build nil
                    :info nil)))

(cl-defun vendle:make-package-name-local (source info)
  (if info
      (thread-first info
        (cl-getf :name)
        (if name (file-name-nondirectory source)))
    (file-name-nondirectory source)))

(cl-defun vendle:make-package-load-path-local (source info)
  (if info
      (cl-letf ((path (cl-getf info :load-path)))
        (if path
            (expand-file-name path source)
          source))
    source))

(cl-defun vendle:make-package-url-local (source info)
  (if info
      (if-let ((url (cl-getf info :url)))
          url "")
    ""))

(provide 'vendle-source-local)

;;; local.el ends here
