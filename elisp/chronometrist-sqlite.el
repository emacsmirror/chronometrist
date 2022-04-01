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
              :custom 'string)
   (connection :initform nil
               :initarg :connection
               :accessor chronometrist-backend-connection)))

(chronometrist-register-backend
 :sqlite "Store records in SQLite database."
 (make-instance 'chronometrist-sqlite-backend :path chronometrist-file))

(cl-defmethod initialize-instance :after ((backend chronometrist-sqlite-backend)
                                          &rest _initargs)
  "Initialize connection for BACKEND based on its file."
  (with-slots (file connection) backend
    (when (and file (not connection))
      (setf connection (emacsql-sqlite file)))))

(cl-defmethod chronometrist-create-file ((backend chronometrist-sqlite-backend) &optional file)
  "Create file for BACKEND if it does not already exist.
Return the connection object from `emacsql-sqlite'."
  (let* ((file (or file (chronometrist-backend-file backend)))
         (db   (or (chronometrist-backend-connection backend)
                   (setf (chronometrist-backend-connection backend)
                         (emacsql-sqlite file)))))
    (cl-loop
      for query in
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
           (name-id integer :not-null :references event-names [name-id])])]
        ;; An interval is a time range with a name and optional properties.
        [:create-table interval-names
         ([(name-id integer :primary-key)
           (name text :unique :not-null)])]
        [:create-table intervals
         ([(interval-id integer :primary-key)
           (name-id integer :not-null :references interval-names [name-id])
           (start-time integer :not-null)
           ;; The latest interval may be ongoing, so the stop time may be NULL.
           (stop-time integer)
           (prop-id integer :references properties [prop-id])]
          (:unique [name-id start-time stop-time]))]
        ;; A date contains one or more events and intervals. It may
        ;; also contain properties.
        [:create-table dates
         ([(date-id integer :primary-key)
           (date integer :unique :not-null)
           (prop-id integer :references properties [prop-id])])]
        [:create-table date-events
         ([(date-id integer :not-null :references dates [date-id])
           (event-id integer :not-null :references events [event-id])])]
        [:create-table date-intervals
         ([(date-id integer :not-null :references dates [date-id])
           (interval-id integer :not-null :references intervals [interval-id])])])
      do (emacsql db query)
      finally return db)))

(defun chronometrist-iso-to-unix (timestamp)
  (truncate (float-time (parse-iso8601-time-string timestamp))))

(cl-defmethod chronometrist-to-file (hash-table (backend chronometrist-sqlite-backend) file)
  (with-slots (connection) backend
    (delete-file file)
    (when connection (emacsql-close connection))
    (setf connection nil)
    (chronometrist-create-file backend file)
    (cl-loop for date in (sort (hash-table-keys hash-table) #'string-lessp) do
      ;; insert date if it does not exist
      (emacsql connection [:insert-or-ignore-into dates [date] :values [$s1]]
               (chronometrist-iso-to-unix date))
      (cl-loop for plist in (gethash date hash-table) do
        (chronometrist-insert backend plist)))))

(defun chronometrist-sqlite-insert-properties (backend plist)
  "Insert properties from PLIST to (SQLite) BACKEND.
Properties are key-values excluding :name, :start, and :stop.

Insert nothing if the properties already exist. Return the
prop-id of the inserted or existing property."
  (with-slots (connection) backend
    (let* ((plist (chronometrist-plist-key-values plist))
           (props (if (functionp chronometrist-sqlite-properties-function)
                      (funcall chronometrist-sqlite-properties-function plist)
                    plist)))
      (emacsql connection
               [:insert-or-ignore-into properties [properties] :values [$s1]]
               props)
      (caar (emacsql connection [:select [prop-id]
                                 :from properties
                                 :where (= properties $s1)]
                     props)))))

(defun chronometrist-sqlite-properties-to-json (plist)
  "Return PLIST as a JSON string."
  (json-encode
   ;; `json-encode' throws an error when it thinks
   ;; it sees "alists" which have numbers as
   ;; "keys", so we convert any cons cells and any
   ;; lists starting with a number to vectors
   (-tree-map (lambda (elt)
                (cond ((chronometrist-pp-pair-p elt)
                       (vector (car elt) (cdr elt)))
                      ((consp elt)
                       (vconcat elt))
                      (t elt)))
              plist)))

(defcustom chronometrist-sqlite-properties-function nil
  "Function used to control the encoding of user key-values.
The function must accept a single argument, the plist of key-values.

Any non-function value results in key-values being inserted as
s-expressions in a text column."
  :type '(choice function (sexp :tag "Insert as s-expressions")))

(cl-defmethod chronometrist-insert ((backend chronometrist-sqlite-backend) plist)
  (-let (((plist-1 plist-2)  (chronometrist-split-plist plist))
         (db  (chronometrist-backend-connection backend)))
    (cl-loop for plist in (if (and plist-1 plist-2)
                              (list plist-1 plist-2)
                            (list plist))
      do
      (-let* (((&plist :name name :start start :stop stop) plist)
              (date-unix   (chronometrist-iso-to-unix (chronometrist-iso-to-date start)))
              (start-unix  (chronometrist-iso-to-unix start))
              (stop-unix   (and stop (chronometrist-iso-to-unix stop)))
              name-id interval-id prop-id)
        ;; insert name if it does not exist
        (emacsql db [:insert-or-ignore-into interval-names [name]
                     :values [$s1]]
                 name)
        ;; insert interval properties if they do not exist
        (setq prop-id (chronometrist-sqlite-insert-properties backend plist))
        ;; insert interval and associate it with the date
        (setq name-id
              (caar (emacsql db [:select [name-id]
                                 :from interval-names
                                 :where (= name $s1)]
                             name)))
        (emacsql db [:insert-or-ignore-into intervals
                     [name-id start-time stop-time prop-id]
                     :values [$s1 $s2 $s3 $s4]]
                 name-id start-unix stop-unix prop-id)
        (emacsql db [:insert-or-ignore-into dates [date]
                     :values [$s1]] date-unix)
        (setq date-id
              (caar (emacsql db [:select [date-id] :from dates
                                 :where (= date $s1)]
                             date-unix))
              interval-id
              (caar (emacsql db [:select (funcall max interval-id) :from intervals])))
        (emacsql db [:insert-into date-intervals [date-id interval-id]
                     :values [$s1 $s2]]
                 date-id interval-id)))))

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

(cl-defmethod chronometrist-replace-last ((backend chronometrist-sqlite-backend) plist)
  (emacsql db [:delete-from events :where ]))

(provide 'chronometrist-sqlite)

;;; chronometrist-sqlite.el ends here
