;;; github.el -*- lexical-binding: t -*-

(cl-defun vendle:source-site-github-p (source)
  (cond
    ((string-match (rx "github:" (submatch (+ (not (in "/")))
                                           "/"
                                           (+ (not (in "/")))))
                   source)
     t)
    ((string-match (rx   line-start
                         (one-or-more (not (in "/")))
                         "/"
                         (one-or-more (not (in "/")))
                         line-end)
                   source)
     t)
    (t nil)))

(cl-defun vendle:source-site-format-github (source)
  (cond
    ((string-match (rx "github:" (submatch (+ (not (in "/")))
                                           "/"
                                           (+ (not (in "/")))))
                   source)
     (match-string-no-properties 1 source))
    ((string-match (rx   line-start
                         (one-or-more (not (in "/")))
                         "/"
                         (one-or-more (not (in "/")))
                         line-end)
                   source)
     (match-string-no-properties 0 source))))

(provide 'vendle-source-github)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
