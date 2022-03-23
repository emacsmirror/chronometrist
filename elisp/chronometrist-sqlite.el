;;; chronometrist-sqlite.el --- SQLite backend for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "24.3") (chronometrist "0.9.0") (emacsql-sqlite "1.0.0"))
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
(require 'emacsql-sqlite)

(defclass chronometrist-sqlite-backend (chronometrist-backend chronometrist-file-backend-mixin)
  ((extension :initform "sqlite"
              :accessor chronometrist-backend-ext
              :custom 'string)))

(chronometrist-register-backend
 :sqlite "Store records in SQLite database."
 (make-instance 'chronometrist-sqlite-backend :path chronometrist-file))

(cl-defmethod chronometrist-to-file (input-hash-table (output-backend chronometrist-sqlite-backend) output-file)
  (with-slots (file) output-backend
    (cl-loop with db = (emacsql-sqlite file)
      with count = 0
      for records being the hash-values of input-hash-table do
      (cl-loop for record in records do
        (chronometrist-insert record db)
        (cl-incf count)
        (when (zerop (% count 5))
          (message "chronometrist-migrate - %s records converted" count)))
      finally return count do
      (message "chronometrist-migrate - finished converting %s events." count))))

(cl-defmethod chronometrist-insert ((backend chronometrist-sqlite-backend) plist)
  (let* ((keywords (seq-filter #'keywordp event))
         (values   (seq-remove #'keywordp event))
         (columns  (mapcar (lambda (keyword)
                             (--> (symbol-name keyword)
                                  (s-chop-prefix ":" it)
                                  ;; emacsql automatically converts
                                  ;; dashes in column names to
                                  ;; underscores, so we do the same,
                                  ;; lest we get a "column already
                                  ;; exists" error
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
Return the emacsql-sqlite connection object."
  (with-slots (file) backend
    (when-let ((db (emacsql-sqlite file)))
      (cl-loop for query in
        '(;; Properties are user-defined key-values stored as JSON.
          [:create-table properties
                         ([(prop-id integer :primary-key)
                           (properties text :unique :not-null)])]
          ;; An event is a timestamp with a name and optional properties.
          [:create-table event-names
                         ([(name-id integer :primary-key)
                           (name text :unique :not-null)])]
          [:create-table events
                         ([(event-id integer :primary-key)
                           (name-id integer :not-null
                                    :references event-names [name-id])])]
          ;; An interval is a time range with a name and optional properties.
          [:create-table interval-names
                         ([(name-id integer :primary-key)
                           (name text :unique :not-null)])]
          [:create-table intervals
                         ([(interval-id integer :primary-key)
                           (name-id integer :not-null
                                    :references interval-names [name-id])
                           (start-time integer :not-null)
                           ;; The latest interval may be ongoing,
                           ;; so the stop time may be NULL.
                           (stop-time integer)
                           (prop-id integer :references properties [prop-id])])]
          ;; A date contains one or more events and intervals. It may
          ;; also contain properties.
          [:create-table dates
                         ([(date-id integer :primary-key)
                           (date integer :unique :not-null)
                           (prop-id integer :references properties [prop-id])])]
          [:create-table date-events
                         ([(date-id integer :not-null
                                    :references dates [date-id])
                           (event-id integer :not-null
                                     :references events [event-id])])]
          [:create-table date-intervals
                         ([(date-id integer :not-null
                                    :references dates [date-id])
                           (interval-id integer :not-null
                                        :references intervals [interval-id])])])
        do (emacsql db query)))))

(cl-defmethod chronometrist-replace-last ((backend chronometrist-sqlite-backend) plist)
  (emacsql db [:delete-from events :where ]))

(provide 'chronometrist-sqlite3)

;;; chronometrist-sqlite3.el ends here
