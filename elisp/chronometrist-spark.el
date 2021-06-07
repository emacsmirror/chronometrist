;;; chronometrist-spark.el --- Show sparklines in Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "24.3") (chronometrist "0.7.0") (spark "0.1"))
;; Version: 0.1.0

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;
;; This package adds a column to Chronometrist displaying sparklines for each task.

;;; Code:
(require 'chronometrist)
(require 'spark)

(defgroup chronometrist-spark nil
  "Show sparklines in `chronometrist'."
  :group 'applications)

(defcustom chronometrist-spark-length 7
  "Length of each sparkline in number of days."
  :type 'integer)

(defcustom chronometrist-spark-show-range t
  "If non-nil, display range of each sparkline."
  :type 'boolean)

(defun chronometrist-spark-row-transformer (row)
  "Add a sparkline cell to ROW.
Used to add a sparkline column to `chronometrist-rows'.

ROW must be a valid element of the list specified by
`tabulated-list-entries'."
  (-let* (((task vector) row)
          (sparkline
           (cl-loop with today = (ts-now)
             with duration
             with active
             for day from (- (- chronometrist-spark-length 1)) to 0
             collect (setq duration
                           (chronometrist-task-time-one-day
                            task (ts-adjust 'day day today)))
             into durations
             if (not (zerop duration))
             do (setq active t)
             finally return
             (let ((duration-minutes (--map (/ it 60) durations)))
               (if (and active chronometrist-spark-show-range)
                   (let ((durations-nonzero (seq-remove #'zerop duration-minutes)))
                     (format "%s (%sm~%sm)"
                             (spark durations)
                             (apply #'min durations-nonzero)
                             (apply #'max duration-minutes)))
                 (format "%s" (spark durations)))))))
    (list task (vconcat vector `[,sparkline]))))

(defun chronometrist-spark-schema-transformer (schema)
  "Add a sparkline column to SCHEMA.
Used to add a sparkline column to `chronometrist-schema-transformers'.
SCHEMA should be a vector as specified by `tabulated-list-format'."
  (vconcat schema `[("Graph"
                     ,(if chronometrist-spark-show-range
                        (+ chronometrist-spark-length 20)
                        chronometrist-spark-length)
                     t)]))

(defun chronometrist-spark-setup ()
  "Add `chronometrist-sparkline' functions to `chronometrist' hooks."
  (add-to-list 'chronometrist-row-transformers     #'chronometrist-spark-row-transformer)
  (add-to-list 'chronometrist-schema-transformers  #'chronometrist-spark-schema-transformer))

(defun chronometrist-spark-teardown ()
  "Remove `chronometrist-sparkline' functions from `chronometrist' hooks."
  (setq chronometrist-row-transformers
        (remove #'chronometrist-spark-row-transformer chronometrist-row-transformers)
        chronometrist-schema-transformers
        (remove #'chronometrist-spark-schema-transformer chronometrist-schema-transformers)))

(define-minor-mode chronometrist-spark-minor-mode
  nil nil nil nil
  ;; when being enabled/disabled, `chronometrist-spark-minor-mode' will already be t/nil here
  (if chronometrist-spark-minor-mode (chronometrist-spark-setup) (chronometrist-spark-teardown)))

(provide 'chronometrist-spark)
;;; chronometrist-spark.el ends here
