;;; github.el -*- lexical-binding: t -*-

(cl-defun vendle:source-site-github-p (source)
  (cond
   ((string-match (rx "github:" (submatch (+ (or (syntax word)
                                                 (syntax symbol)
                                                 (syntax punctuation)))
                                          "/"
                                          (+ (or (syntax word)
                                                 (syntax symbol)
                                                 (syntax punctuation)))))
                  source)
    t)
   ((string-match (rx   line-start
                        (one-or-more (or (syntax symbol)
                                         (syntax word)
                                         (syntax punctuation)))
                        "/"
                        (one-or-more (or (syntax symbol)
                                         (syntax word)
                                         (syntax punctuation)))
                        line-end)
                  source)
    t)
   (t nil)))

(cl-defun vendle:source-site-format-github (source)
  (cond
   ((string-match (rx "github:" (submatch (+ (or (syntax word) (syntax symbol) (syntax punctuation)))
                                          "/"
                                          (+ (or (syntax word) (syntax symbol) (syntax punctuation)))))
                  source)
    (match-string-no-properties 1 source))
   ((string-match (rx   line-start
                        (one-or-more (or (syntax symbol) (syntax word) (syntax punctuation)))
                        "/"
                        (one-or-more (or (syntax symbol)
                                         (syntax word)
                                         (syntax punctuation)))
                        line-end)
                  source)
    (match-string-no-properties 0 source))))

(provide 'vendle-site-github)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
