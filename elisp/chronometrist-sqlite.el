;;; chronometrist-sql.el --- SQL backends for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "24.3") (chronometrist "0.9.0") (spark "0.1"))
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
;; This package provides an SQLite 3 backend for Chronometrist.

;;; Code:
(require 'chronometrist)
(require 'emacsql-sqlite3)

(defclass chronometrist-sqlite-backend (chronometrist-backend chronometrist-file-backend-mixin)
  ((extension :initform "sqlite3"
              :accessor chronometrist-backend-ext
              :custom 'string)))

(cl-defmethod chronometrist-to-file (input-hash-table (output-backend chronometrist-sqlite-backend) output-file)
  (cl-loop with db = (emacsql-sqlite3 (concat file "." (oref backend :ext)))
    with count = 0
    for events being the hash-values of hash-table do
    (cl-loop for event in events do
      (chronometrist-insert event db)
      (incf count)
      (when (zerop (% count 5))
        (message "chronometrist-migrate - %s events converted" count)))
    finally return count do
    (message "chronometrist-migrate - finished converting %s events." count)))

(cl-defmethod chronometrist-insert ((backend chronometrist-sqlite-backend) plist)
  (let* ((keywords (seq-filter #'keywordp event))
         (values   (seq-remove #'keywordp event))
         (columns  (mapcar (lambda (keyword)
                             (--> (symbol-name keyword)
                                  (s-chop-prefix ":" it)
                                  ;; emacsql seems to automatically
                                  ;; convert dashes in column names
                                  ;; to underscores, so we do the
                                  ;; same, lest we get a "column
                                  ;; already exists" error
                                  (replace-regexp-in-string "-" "_" it)
                                  (intern it)))
                           keywords)))
    ;; ensure all keywords in this plist exist as SQL columns
    (cl-loop for column in columns do
      (let* ((pragma        (emacsql db [:pragma (funcall table_info events)]))
             (column-exists (cl-loop for column-spec in pragma thereis
                              (eq column (second column-spec)))))
        (unless column-exists
          (emacsql db [:alter-table events :add-column $i1] column))))
    (emacsql db [:insert-into events [$i1] :values $v2]
             (vconcat columns) (vconcat values))))

(cl-defmethod chronometrist-edit-backend ((backend chronometrist-sqlite-backend))
  (require 'sql)
  (switch-to-buffer
   (sql-comint-sqlite 'sqlite (list file))))

;; SELECT * FROM TABLE WHERE ID = (SELECT MAX(ID) FROM TABLE);
;; SELECT * FROM tablename ORDER BY column DESC LIMIT 1;
(cl-defmethod chronometrist-latest-record ((backend chronometrist-sqlite-backend) db)
  (emacsql db [:select * :from events :order-by rowid :desc :limit 1]))

(cl-defmethod chronometrist-task-records-for-date ((backend chronometrist-sqlite-backend) task date-ts))

(cl-defmethod chronometrist-active-days ((backend chronometrist-sqlite-backend) task))

(cl-defmethod chronometrist-create-file ((backend chronometrist-sqlite-backend))
  "Create file for BACKEND if it does not already exist.
Return the emacsql-sqlite3 connection object."
  (aprog1 (emacsql-sqlite3 (concat chronometrist-file "." (oref backend :ext)))
    (emacsql it [:create-table events ([name tags start stop])])))

(cl-defmethod chronometrist-replace-last ((backend chronometrist-sqlite-backend) plist)
  (emacsql db [:delete-from events :where ]))

(provide 'chronometrist-sqlite3)

;;; chronometrist-sqlite3.el ends here
