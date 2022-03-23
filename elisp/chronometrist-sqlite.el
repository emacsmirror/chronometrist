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

;; predicate to find prop-id for property if it exists
;; insert property if it does not exist (procedure)
;; insert date if it does not exist (procedure)
;; insert event (generic)
;; insert interval (generic)
;; insert date properties (generic)
(defun chronometrist-sqlite-insert-properties (backend plist)
  "Insert properties from PLIST to BACKEND.
Properties are key-values excluding :name, :start, and :stop.

Insert nothing if the properties already exist. Return the
prop-id of the inserted or existing property."
  (with-slots (file) backend
    (-let* ((db   (emacsql-sqlite file))
            (json (json-encode (chronometrist-plist-key-values plist)))
            ((results &as (prop-id))
             (emacsql db [:select [prop-id]
                          :from properties
                          :where (= properties $s1)]
                      json)))
      (if prop-id
          prop-id
        (emacsql db [:insert-into properties [properties] :values [$s1]] json)
        (last (emacsql db [:select [prop-id] :from properties]))))))

(cl-defmethod chronometrist-insert ((backend chronometrist-sqlite-backend) plist)
  (-let* (((&plist :name name :start start :stop stop) plist)
          ((name-results &as (name-id))
           (emacsql db [:select [name-id] :from interval-names
                                :where (= name $s1)]
                    name))
          (start-unix    (chronometrist-iso-to-unix start))
          (stop-unix     (and stop (chronometrist-iso-to-unix stop))))
    ;; insert name if it does not exist
    (unless name-id
      (emacsql db [:insert-into interval-names [name] :values [$s1]] name))
    ;; XXX - insert interval properties if they do not exist
    ;; insert interval and associate it with the date
    (emacsql db [:insert-into intervals [name-id start-time stop-time]
                              :values [$s1 $s2 $s3]]
             name-id start-unix stop-unix)
    (emacsql db [:insert-into date-intervals [date-id interval-id]
                              :values [$s1 $s2]]
             date-id
             ;; the newest interval-id
             )))

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

(defun chronometrist-iso-to-unix (timestamp)
  (truncate (float-time (parse-iso8601-time-string timestamp))))

(cl-defmethod chronometrist-to-file (hash-table (backend chronometrist-sqlite-backend) file)
  (delete-file file)
  (chronometrist-create-file backend file)
  (when-let ((db (emacsql-sqlite file)))
    (cl-loop for date in (sort (hash-table-keys hash-table) #'string-lessp) do
      ;; insert date if it does not exist
      (-let* ((date-unix     (chronometrist-iso-to-unix date))
              ((date-results &as (date-id))
               (emacsql db [:select [date-id] :from dates :where (= date $s1)]
                        date-unix)))
        (unless date-results
          (emacsql db [:insert-into dates [date] :values [$s1]] date-unix))
        ;; XXX - insert date properties
        (cl-loop for plist in (gethash date hash-table) do
          (chronometrist-insert backend plist)
          ;; XXX - insert events
          ))
      )))

(cl-defmethod chronometrist-replace-last ((backend chronometrist-sqlite-backend) plist)
  (emacsql db [:delete-from events :where ]))

(provide 'chronometrist-sqlite3)

;;; chronometrist-sqlite3.el ends here
