;;; github.el -*- lexical-binding: t -*-

(cl-defun vendle:source-github-p (source)
  (cond ((string-match (rx "github:" (submatch (+ (or (syntax word) (syntax symbol)))
                                               "/"
                                               (+ (or (syntax word) (syntax symbol)))))
                       source)
         t)
        ((string-match (rx   line-start
                             (one-or-more (or (syntax symbol) (syntax word)))
                             "/"
                             (one-or-more (or (syntax symbol)
                                              (syntax word)))
                             line-end)
                       source)
         t)
        (t nil)))

(cl-defun vendle:source-format-github (source)
  (cond
   ((string-match (rx "github:" (submatch (+ (or (syntax word) (syntax symbol)))
                                          "/"
                                          (+ (or (syntax word) (syntax symbol)))))
                  source)
    (match-string-no-properties 1 source))
   ((string-match (rx   line-start
                        (one-or-more (or (syntax symbol) (syntax word)))
                        "/"
                        (one-or-more (or (syntax symbol)
                                         (syntax word)))
                        line-end)
                  source)
    (match-string-no-properties 0 source))))

(provide 'vendle-source-github)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
