;;; chronometrist.el --- Friendly and powerful personal time tracker and analyzer -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "27.1") (dash "2.16.0") (seq "2.20") (ts "0.2"))
;; Version: 0.9.0

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
;; Friendly and powerful personal time tracker and analyzer.

;; Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

;; * Benefits
;;   1. Extremely simple and efficient to use
;;   2. Displays useful information about your time usage
;;   3. Support for both mouse and keyboard
;;   4. Human errors in tracking are easily fixed by editing a plain text file
;;   5. Hooks to let you perform arbitrary actions when starting/stopping tasks

;; * Limitations
;;   1. No support (yet) for adding a task without clocking into it.
;;   2. No support for concurrent tasks.

;; ## Comparisons
;; ### timeclock.el
;; Compared to timeclock.el, Chronometrist
;; * stores data in an s-expression format rather than a line-based one
;; * supports attaching tags and arbitrary key-values to time intervals
;; * has commands to shows useful summaries
;; * has more hooks

;; ### Org time tracking
;; Chronometrist and Org time tracking seem to be equivalent in terms of capabilities, approaching the same ends through different means.
;; * Chronometrist doesn't have a mode line indicator at the moment. (planned)
;; * Chronometrist doesn't have Org's sophisticated querying facilities. (an SQLite backend is planned)
;; * Org does so many things that keybindings seem to necessarily get longer. Chronometrist has far fewer commands than Org, so most of the keybindings are single keys, without modifiers.
;; * Chronometrist's UI makes keybindings discoverable - they are displayed in the buffers themselves.
;; * Chronometrist's UI is cleaner, since the storage is separate from the display. It doesn't show tasks as trees like Org, but it uses tags and key-values to achieve that. Additionally, navigating a flat list takes fewer user operations than navigating a tree.
;; * Chronometrist data is just s-expressions (plists), and may be easier to parse than a complex text format with numerous use-cases.

;; For information on usage and customization, see https://tildegit.org/contrapunctus/chronometrist or the included manual.org

;;; Code:
;; This file was automatically generated from chronometrist.org.
(require 'dash)
(require 'ts)

(require 'cl-lib)
(require 'seq)
(require 'filenotify)
(require 'subr-x)
(require 'parse-time)
(require 'eieio)

(eval-when-compile
  (defvar chronometrist-mode-map)
  (require 'subr-x))

;; [[file:chronometrist.org::*custom group][custom group:1]]
(defgroup chronometrist nil
  "An extensible time tracker."
  :group 'applications)
;; custom group:1 ends here

;; [[file:chronometrist.org::*format-time][format-time:1]]
(cl-defun chronometrist-format-duration (seconds &optional (blank (make-string 3 ?\s)))
  "Format SECONDS as a string suitable for display in Chronometrist buffers.
SECONDS must be a positive integer.

BLANK is a string to display in place of blank values. If not
supplied, 3 spaces are used."
  (-let [(h m s) (chronometrist-seconds-to-hms seconds)]
    (if (and (zerop h) (zerop m) (zerop s))
        (concat (make-string 7 ?\s) "-")
      (let ((h (if (zerop h) blank (format "%2d:" h)))
            (m (cond ((and (zerop h) (zerop m))  blank)
                     ((zerop h)  (format "%2d:" m))
                     (t  (format "%02d:" m))))
            (s (if (and (zerop h) (zerop m))
                   (format "%2d" s)
                 (format "%02d" s))))
        (concat h m s)))))
;; format-time:1 ends here

;; [[file:chronometrist.org::*file-empty-p][file-empty-p:1]]
(defun chronometrist-file-empty-p (file)
  "Return t if FILE is empty."
  (zerop (nth 7 (file-attributes file))))
;; file-empty-p:1 ends here

;; [[file:chronometrist.org::*format-keybinds][format-keybinds:1]]
(defun chronometrist-format-keybinds (command map &optional firstonly)
  "Return the keybindings for COMMAND in MAP as a string.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (if firstonly
      (key-description
       (where-is-internal command map firstonly))
    (->> (where-is-internal command map)
         (mapcar #'key-description)
         (-take 2)
         (-interpose ", ")
         (apply #'concat))))
;; format-keybinds:1 ends here

;; [[file:chronometrist.org::*day-start-time][day-start-time:1]]
(defcustom chronometrist-day-start-time "00:00:00"
  "The time at which a day is considered to start, in \"HH:MM:SS\".

The default is midnight, i.e. \"00:00:00\"."
  :type 'string)
;; day-start-time:1 ends here

;; [[file:chronometrist.org::*week-start-day][week-start-day:1]]
(defcustom chronometrist-report-week-start-day "Sunday"
  "The day used for start of week by `chronometrist-report'."
  :type 'string)
;; week-start-day:1 ends here

;; [[file:chronometrist.org::*weekday-number-alist][weekday-number-alist:1]]
(defcustom chronometrist-report-weekday-number-alist
  '(("Sunday"    . 0)
    ("Monday"    . 1)
    ("Tuesday"   . 2)
    ("Wednesday" . 3)
    ("Thursday"  . 4)
    ("Friday"    . 5)
    ("Saturday"  . 6))
  "Alist in the form (\"NAME\" . NUMBER), where \"NAME\" is the name of a weekday and NUMBER its associated number."
  :type 'alist)
;; weekday-number-alist:1 ends here

;; [[file:chronometrist.org::*previous-week-start][previous-week-start:1]]
(defun chronometrist-previous-week-start (ts)
  "Find the previous `chronometrist-report-week-start-day' from TS.
Return a ts struct for said day's beginning.

If the day of TS is the same as the
`chronometrist-report-week-start-day', return TS.

TS must be a ts struct (see `ts.el')."
  (cl-loop with week-start = (alist-get chronometrist-report-week-start-day
                                        chronometrist-report-weekday-number-alist
                                        nil nil #'equal)
    until (= week-start (ts-dow ts))
    do (ts-decf (ts-day ts))
    finally return ts))
;; previous-week-start:1 ends here

;; [[file:chronometrist.org::*plist-remove][plist-remove:1]]
(defun chronometrist-plist-remove (plist &rest keys)
  "Return PLIST with KEYS and their associated values removed."
  (let ((keys (--filter (plist-member plist it) keys)))
    (mapc (lambda (key)
            (let ((pos (seq-position plist key)))
              (setq plist (append (seq-take plist pos)
                                  (seq-drop plist (+ 2 pos))))))
          keys)
    plist))
;; plist-remove:1 ends here

;; [[file:chronometrist.org::*plist-key-values][plist-key-values:1]]
(defun chronometrist-plist-key-values (plist)
  "Return user key-values from PLIST."
  (chronometrist-plist-remove plist :name :tags :start :stop))
;; plist-key-values:1 ends here

;; [[file:chronometrist.org::*plist-p][plist-p:1]]
(defun chronometrist-plist-p (list)
  "Return non-nil if LIST is a property list, i.e. (:KEYWORD VALUE ...)"
  (when list
    (while (consp list)
      (setq list (if (and (keywordp (cl-first list)) (consp (cl-rest list)))
                     (cddr list)
                   'not-plist)))
    (null list)))
;; plist-p:1 ends here

;; [[file:chronometrist.org::*plist type][plist type:1]]
(cl-deftype chronometrist-plist ()
  '(satisfies chronometrist-plist-p)
  '(satisfies (lambda (plist)
                (and (plist-get plist :name)
                     (plist-get plist :start)))))
;; plist type:1 ends here

;; [[file:chronometrist.org::*delete-list][delete-list:1]]
(defun chronometrist-sexp-delete-list (&optional arg)
  "Delete ARG lists after point.
Return new position of point."
  (let ((point-1 (point)))
    ;; try to preserve the file-local variable prop line in case this
    ;; is run from the start of buffer
    (while (forward-comment 1) nil)
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))
    (point)))
;; delete-list:1 ends here

;; [[file:chronometrist.org::*make-hash-table][make-hash-table:1]]
(defun chronometrist-make-hash-table ()
  "Return an empty hash table with `equal' as test."
  (make-hash-table :test #'equal))
;; make-hash-table:1 ends here

;; [[file:chronometrist.org::*current-task][current-task:1]]
(cl-defun chronometrist-current-task (&optional (backend (chronometrist-active-backend)))
  "Return the name of the active task as a string, or nil if not clocked in."
  (let ((last-event (chronometrist-latest-record backend)))
    (if (plist-member last-event :stop)
        nil
      (plist-get last-event :name))))
;; current-task:1 ends here

;; [[file:chronometrist.org::*install-directory][install-directory:1]]
(defvar chronometrist-install-directory
  (when load-file-name
    (file-name-directory load-file-name))
  "Directory where Chronometrist has been installed.")
;; install-directory:1 ends here

;; [[file:chronometrist.org::*doc-paths][doc-paths:1]]
(defvar chronometrist-doc-paths '(:lp "chronometrist.org")
  "Plist of names of Chronometrist's documentation files.")
;; doc-paths:1 ends here

;; [[file:chronometrist.org::*open-literate-source][open-literate-source:1]]
(defun chronometrist-open-literate-source ()
  "Visit the Org literate program for Chronometrist."
  (interactive)
  (find-file (concat chronometrist-install-directory (plist-get chronometrist-doc-paths :lp))))
;; open-literate-source:1 ends here

;; [[file:chronometrist.org::*debug logging][debug logging:1]]
(defcustom chronometrist-debug-enable nil
  "Whether to log debugging messages."
  :type 'boolean
  :group 'chronometrist)

(defcustom chronometrist-debug-buffer "*chronometrist-debug*"
  "Name of buffer to log debug messages to."
  :type 'string
  :group 'chronometrist)

(define-derived-mode chronometrist-debug-log-mode special-mode "debug-log")

(defun chronometrist-debug-message (format-string &rest args)
  "Log a debug message to `chronometrist-debug-buffer'.
FORMAT-STRING and ARGS are passed to `format'."
  (when chronometrist-debug-enable
    (with-current-buffer (get-buffer-create chronometrist-debug-buffer)
      (goto-char (point-max))
      (chronometrist-debug-log-mode)
      (let ((inhibit-read-only t))
        (insert
         (apply #'format
                (concat (format-time-string "[%T] ")
                        format-string)
                args)
         "\n")))))
;; debug logging:1 ends here

;; [[file:chronometrist.org::*reset][reset:1]]
(defun chronometrist-reset ()
  "Reset Chronometrist's internal state."
  (interactive)
  (chronometrist-debug-message "[Command] reset")
  (chronometrist-reset-backend (chronometrist-active-backend))
  (chronometrist-refresh))
;; reset:1 ends here

;; [[file:chronometrist.org::*apply-time][apply-time:1]]
(defun chronometrist-apply-time (time timestamp)
  "Return TIMESTAMP with time modified to TIME.
TIME must be a string in the form \"HH:MM:SS\"

TIMESTAMP must be a time string in the ISO-8601 format.

Return value is a ts struct (see `ts.el')."
  (-let [(h m s) (mapcar #'string-to-number (split-string time ":"))]
    (ts-apply :hour h :minute m :second s
              (chronometrist-iso-to-ts timestamp))))
;; apply-time:1 ends here

;; [[file:chronometrist.org::*split-plist][split-plist:1]]
(defun chronometrist-split-plist (plist)
  "Return a list of two split plists if PLIST spans a midnight, else nil."
  (when (plist-get plist :stop)
    (let ((split-time (chronometrist-split-time (plist-get plist :start)
                                           (plist-get plist :stop)
                                           chronometrist-day-start-time)))
      (when split-time
        (-let* (((&plist :start start-1 :stop stop-1) (cl-first  split-time))
                ((&plist :start start-2 :stop stop-2) (cl-second split-time))
                ;; `plist-put' modifies lists in-place. The resulting bugs
                ;; left me puzzled for a while.
                (event-1      (cl-copy-list plist))
                (event-2      (cl-copy-list plist)))
          (list (-> event-1
                    (plist-put :start start-1)
                    (plist-put :stop  stop-1))
                (-> event-2
                    (plist-put :start start-2)
                    (plist-put :stop  stop-2))))))))
;; split-plist:1 ends here

;; [[file:chronometrist.org::*events-update][events-update:1]]
(defun chronometrist-events-update (plist hash-table &optional replace)
  "Return HASH-TABLE with PLIST added as the latest interval.
If REPLACE is non-nil, replace the last interval with PLIST."
  (let* ((date (->> (plist-get plist :start)
                    (chronometrist-iso-to-ts )
                    (ts-format "%F" )))
         (events-today (gethash date hash-table)))
    (--> (if replace (-drop-last 1 events-today) events-today)
         (append it (list plist))
         (puthash date it hash-table))
    hash-table))
;; events-update:1 ends here

;; [[file:chronometrist.org::*last-date][last-date:1]]
(defun chronometrist-events-last-date (hash-table)
  "Return an ISO-8601 date string for the latest date present in `chronometrist-events'."
  (--> (hash-table-keys hash-table)
       (last it)
       (car it)))
;; last-date:1 ends here

;; [[file:chronometrist.org::*events-last][events-last:1]]
(cl-defun chronometrist-events-last (&optional (backend (chronometrist-active-backend)))
  "Return the last plist from `chronometrist-events'."
  (let* ((hash-table (chronometrist-backend-hash-table backend))
         (last-date  (chronometrist-events-last-date hash-table)))
    (--> (gethash last-date hash-table)
         (last it)
         (car it))))
;; events-last:1 ends here

;; [[file:chronometrist.org::#program-data-structures-events-subset][events-subset:1]]
(defun chronometrist-events-subset (start end hash-table)
  "Return a subset of HASH-TABLE.
The subset will contain values between dates START and END (both
inclusive).

START and END must be ts structs (see `ts.el'). They will be
treated as though their time is 00:00:00."
  (let ((subset (chronometrist-make-hash-table))
        (start  (chronometrist-date-ts start))
        (end    (chronometrist-date-ts end)))
    (maphash (lambda (key value)
               (when (ts-in start end (chronometrist-iso-to-ts key))
                 (puthash key value subset)))
             hash-table)
    subset))
;; events-subset:1 ends here

;; [[file:chronometrist.org::*task-time-one-day][task-time-one-day:1]]
(cl-defun chronometrist-task-time-one-day (task &optional (date (chronometrist-date-ts)) (backend (chronometrist-active-backend)))
  "Return total time spent on TASK today or on DATE, an ISO-8601 date.
The return value is seconds, as an integer."
  (let ((task-events (chronometrist-task-records-for-date backend task date)))
    (if task-events
        (->> (chronometrist-events-to-durations task-events)
             (-reduce #'+)
             (truncate))
      ;; no events for this task on DATE, i.e. no time spent
      0)))
;; task-time-one-day:1 ends here

;; [[file:chronometrist.org::*active-time-on][active-time-on:1]]
(defvar chronometrist-task-list)
(cl-defun chronometrist-active-time-on (&optional (date (chronometrist-date-ts)))
  "Return the total active time today, or on DATE.
Return value is seconds as an integer."
  (->> (--map (chronometrist-task-time-one-day it date) (chronometrist-task-list))
       (-reduce #'+)
       (truncate)))
;; active-time-on:1 ends here

;; [[file:chronometrist.org::*count-active-days][count-active-days:1]]
(cl-defun chronometrist-statistics-count-active-days (task table)
  "Return the number of days the user spent any time on TASK.
  TABLE must be a hash table - if not supplied, `chronometrist-events' is used.

  This will not return correct results if TABLE contains records
which span midnights."
  (cl-loop for events being the hash-values of table
    count (seq-find (lambda (event)
                      (equal task (plist-get event :name)))
                    events)))
;; count-active-days:1 ends here

;; [[file:chronometrist.org::*task-list][task-list:1]]
(defcustom chronometrist-task-list nil
  "List of tasks used by `chronometrist'.
Value may be either nil or a list of strings.

If nil, the task list is generated from user data in
`chronometrist-file' and stored in the task-list slot of the
active backend."
  :type '(choice (repeat string) nil)
  :group 'chronometrist)
;; task-list:1 ends here

;; [[file:chronometrist.org::*iso-to-ts][iso-to-ts:1]]
(defun chronometrist-iso-to-ts (timestamp)
  "Convert TIMESTAMP to a TS struct. (see `ts.el')
TIMESTAMP must be an ISO-8601 timestamp, as handled by
`parse-iso8601-time-string'."
  (-let [(second minute hour day month year dow _dst utcoff)
         (decode-time
          (parse-iso8601-time-string timestamp))]
    (ts-update
     (make-ts :hour hour :minute minute :second second
              :day day   :month month   :year year
              :dow dow   :tz-offset utcoff))))
;; iso-to-ts:1 ends here

;; [[file:chronometrist.org::*events-to-durations][events-to-durations:1]]
(defun chronometrist-events-to-durations (events)
  "Convert EVENTS into a list of durations in seconds.
EVENTS must be a list of valid Chronometrist property lists (see
`chronometrist-file').

Return 0 if EVENTS is nil."
  (if events
      (cl-loop for plist in events collect
        (let* ((start-ts (chronometrist-iso-to-ts
                          (plist-get plist :start)))
               (stop-iso (plist-get plist :stop))
               ;; Add a stop time if it does not exist.
               (stop-ts  (if stop-iso
                             (chronometrist-iso-to-ts stop-iso)
                           (ts-now))))
          (ts-diff stop-ts start-ts)))
    0))
;; events-to-durations:1 ends here

;; [[file:chronometrist.org::*date-iso][date-iso:1]]
(cl-defun chronometrist-date-iso (&optional (ts (ts-now)))
  (ts-format "%F" ts))
;; date-iso:1 ends here

;; [[file:chronometrist.org::*date-ts][date-ts:1]]
(cl-defun chronometrist-date-ts (&optional (ts (ts-now)))
  "Return a ts struct representing the time 00:00:00 on today's date.
If TS is supplied, use that date instead of today.
TS should be a ts struct (see `ts.el')."
  (ts-apply :hour 0 :minute 0 :second 0 ts))
;; date-ts:1 ends here

;; [[file:chronometrist.org::*format-time-iso8601][format-time-iso8601:1]]
(defun chronometrist-format-time-iso8601 (&optional unix-time)
  "Return current date and time as an ISO-8601 timestamp.
Optional argument UNIX-TIME should be a time value (see
`current-time') accepted by `format-time-string'."
  (format-time-string "%FT%T%z" unix-time))

;; Note - this assumes that an event never crosses >1 day. This seems
;; sufficient for all conceivable cases.
;; format-time-iso8601:1 ends here

;; [[file:chronometrist.org::*split-time][split-time:1]]
(defun chronometrist-split-time (start-time stop-time day-start-time)
  "If START-TIME and STOP-TIME intersect DAY-START-TIME, split them into two intervals.
START-TIME and STOP-TIME must be ISO-8601 timestamps e.g. \"YYYY-MM-DDTHH:MM:SSZ\".

DAY-START-TIME must be a string in the form \"HH:MM:SS\" (see
`chronometrist-day-start-time')

Return a list in the form
\((:start START-TIME
  :stop <day-start time on initial day>)
 (:start <day start time on second day>
  :stop STOP-TIME))"
  ;; FIXME - time zones are ignored; may cause issues with
  ;; time-zone-spanning events

  ;; The time on which the first provided day starts (according to `chronometrist-day-start-time')
  (let* ((stop-ts         (chronometrist-iso-to-ts stop-time))
         (first-day-start (chronometrist-apply-time day-start-time start-time))
         (next-day-start  (ts-adjust 'hour 24 first-day-start)))
    ;; Does the event stop time exceed the next day start time?
    (when (ts< next-day-start stop-ts)
      (let ((split-time (ts-format "%FT%T%z" next-day-start)))
        (list `(:start ,start-time :stop ,split-time)
              `(:start ,split-time :stop ,stop-time))))))
;; split-time:1 ends here

;; [[file:chronometrist.org::*seconds-to-hms][seconds-to-hms:1]]
(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES SECONDS].
SECONDS must be a positive integer."
  (let* ((seconds (truncate seconds))
         (s       (% seconds 60))
         (m       (% (/ seconds 60) 60))
         (h       (/ seconds 3600)))
    (list h m s)))
;; seconds-to-hms:1 ends here

;; [[file:chronometrist.org::*interval][interval:1]]
(defun chronometrist-interval (event)
  "Return the period of time covered by EVENT as a time value.
EVENT should be a plist (see `chronometrist-file')."
  (let ((start (plist-get event :start))
        (stop  (plist-get event :stop)))
    (time-subtract (parse-iso8601-time-string stop)
                   (parse-iso8601-time-string start))))
;; interval:1 ends here

;; [[file:chronometrist.org::*format-duration-long][format-duration-long:1]]
(defun chronometrist-format-duration-long (seconds)
  "Return SECONDS as a human-friendly duration string.
e.g. \"2 hours, 10 minutes\". SECONDS must be an integer. If
SECONDS is less than 60, return a blank string."
  (let* ((hours         (/ seconds 60 60))
         (minutes       (% (/ seconds 60) 60))
         (hour-string   (if (= 1 hours) "hour" "hours"))
         (minute-string (if (= 1 minutes) "minute" "minutes")))
    (cond ((and (zerop hours) (zerop minutes)) "")
          ((zerop hours)
           (format "%s %s" minutes minute-string))
          ((zerop minutes)
           (format "%s %s" hours hour-string))
          (t (format "%s %s, %s %s"
                     hours hour-string
                     minutes minute-string)))))
;; format-duration-long:1 ends here

;; [[file:chronometrist.org::*normalize-whitespace][normalize-whitespace:1]]
(defun chronometrist-pp-normalize-whitespace ()
  "Remove whitespace following point, and insert a space.
Point is placed at the end of the space."
  (when (looking-at "[[:blank:]]+")
    (delete-region (match-beginning 0) (match-end 0))
    (insert " ")))
;; normalize-whitespace:1 ends here

;; [[file:chronometrist.org::*column][column:1]]
(defun chronometrist-pp-column ()
  "Return column point is on, as an integer.
0 means point is at the beginning of the line."
  (- (point) (point-at-bol)))
;; column:1 ends here

;; [[file:chronometrist.org::*pair-p][pair-p:1]]
(defun chronometrist-pp-pair-p (cons)
  "Return non-nil if CONS is a pair, i.e. (CAR . CDR)."
  (and (listp cons) (not (listp (cdr cons)))))
;; pair-p:1 ends here

;; [[file:chronometrist.org::*alist-p][alist-p:1]]
(defun chronometrist-pp-alist-p (list)
  "Return non-nil if LIST is an association list.
If even a single element of LIST is a pure cons cell (as
determined by `chronometrist-pp-pair-p'), this function
considers it an alist."
  (when (listp list)
    (cl-loop for elt in list thereis (chronometrist-pp-pair-p elt))))
;; alist-p:1 ends here

;; [[file:chronometrist.org::*plist-group-p][plist-group-p:1]]
(defun chronometrist-plist-group-p (list)
  "Return non-nil if LIST is in the form \(ATOM PLIST+\)."
  (and (consp list)
       (not (consp (cl-first list)))
       (cl-rest list)
       (seq-every-p #'chronometrist-plist-p (cl-rest list))))
;; plist-group-p:1 ends here

;; [[file:chronometrist.org::*longest-keyword-length][longest-keyword-length:1]]
(defun chronometrist-pp-longest-keyword-length ()
  "Find the length of the longest keyword in a plist.
This assumes there is a single plist in the current buffer, and
that point is after the first opening parenthesis."
  (save-excursion
    (cl-loop with sexp
      while (setq sexp (ignore-errors (read (current-buffer))))
      when (keywordp sexp)
      maximize (length (symbol-name sexp)))))
;; longest-keyword-length:1 ends here

;; [[file:chronometrist.org::*indent-sexp][indent-sexp:1]]
(cl-defun chronometrist-pp-indent-sexp (sexp &optional (right-indent 0))
  "Return a string indenting SEXP by RIGHT-INDENT spaces."
  (format (concat "% -" (number-to-string right-indent) "s")
          sexp))
;; indent-sexp:1 ends here

;; [[file:chronometrist.org::*buffer][buffer:1]]
(cl-defun chronometrist-pp-buffer (&optional in-sublist)
  "Recursively indent the alist, plist, or a list of plists after point.
The list must be on a single line, as emitted by `prin1'.

IN-SUBLIST, if non-nil, means point is inside an inner list."
  (if (not (looking-at-p (rx (or ")" line-end))))
      (let ((sexp (save-excursion
                    (read (current-buffer)))))
          (cond
           ((chronometrist-plist-p sexp)
            (chronometrist-pp-buffer-plist in-sublist)
            ;; we want to continue, in case we were inside a sublist
            (chronometrist-pp-buffer in-sublist))
           ((chronometrist-plist-group-p sexp)
            (chronometrist-pp-buffer-plist-group in-sublist)
            (chronometrist-pp-buffer in-sublist))
           ((chronometrist-pp-alist-p sexp)
            (chronometrist-pp-buffer-alist)
            (unless in-sublist (chronometrist-pp-buffer)))
           ((chronometrist-pp-pair-p sexp)
            (forward-sexp)
            (chronometrist-pp-buffer in-sublist))
           ((listp sexp)
            (down-list)
            (chronometrist-pp-buffer t)
            (up-list))
           ;; atoms and other values
           (t (forward-sexp)
              (chronometrist-pp-buffer in-sublist))))
    ;; we're before a ) - is it a lone paren on its own line?
    (let ((point (point))
          (bol   (point-at-bol)))
      (goto-char bol)
      (if (string-match "^[[:blank:]]*$" (buffer-substring bol point))
          ;; join the ) to the previous line by deleting the newline and whitespace
          (delete-region (1- bol) point)
        (goto-char point)))))
;; buffer:1 ends here

;; [[file:chronometrist.org::*buffer-plist][buffer-plist:1]]
(defun chronometrist-pp-buffer-plist (&optional in-sublist)
  "Indent a single plist after point."
  (down-list)
  (let ((left-indent  (1- (chronometrist-pp-column)))
        (right-indent (chronometrist-pp-longest-keyword-length))
        (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (chronometrist-pp-normalize-whitespace)
      (setq sexp (save-excursion (read (current-buffer))))
      (cond ((keywordp sexp)
             (chronometrist-sexp-delete-list)
             (insert (if first-p
                         (progn (setq first-p nil) "")
                       (make-string left-indent ?\s))
                     (chronometrist-pp-indent-sexp sexp right-indent)))
            ;; not a keyword = a value
            ((chronometrist-plist-p sexp)
             (chronometrist-pp-buffer-plist))
            ((and (listp sexp)
                  (not (chronometrist-pp-pair-p sexp)))
             (chronometrist-pp-buffer t)
             (insert "\n"))
            (t (forward-sexp)
               (insert "\n"))))
    (when (bolp) (delete-char -1))
    (up-list)
    ;; we have exited the plist, but might still be in a list with more plists
    (unless (eolp) (insert "\n"))
    (when in-sublist
      (insert (make-string (1- left-indent) ?\s)))))
;; buffer-plist:1 ends here

;; [[file:chronometrist.org::*buffer-plist-group][buffer-plist-group:1]]
(defun chronometrist-pp-buffer-plist-group (&optional _in-sublist)
  (down-list)
  (forward-sexp)
  (default-indent-new-line)
  (chronometrist-pp-buffer t))
;; buffer-plist-group:1 ends here

;; [[file:chronometrist.org::*buffer-alist][buffer-alist:1]]
(defun chronometrist-pp-buffer-alist ()
  "Indent a single alist after point."
  (down-list)
  (let ((indent (chronometrist-pp-column)) (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (setq sexp (save-excursion (read (current-buffer))))
      (chronometrist-sexp-delete-list)
      (insert (if first-p
                  (progn (setq first-p nil) "")
                (make-string indent ?\s))
              (format "%S\n" sexp)))
    (when (bolp) (delete-char -1))
    (up-list)))
;; buffer-alist:1 ends here

;; [[file:chronometrist.org::*to-string][to-string:1]]
(defun chronometrist-pp-to-string (object)
  "Convert OBJECT to a pretty-printed string."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-quoted t))
      (prin1 object (current-buffer)))
    (goto-char (point-min))
    (chronometrist-pp-buffer)
    (buffer-string)))
;; to-string:1 ends here

;; [[file:chronometrist.org::plist-pp][plist-pp]]
(defun chronometrist-plist-pp (object &optional stream)
  "Pretty-print OBJECT and output to STREAM (see `princ')."
  (princ (chronometrist-pp-to-string object)
         (or stream standard-output)))
;; plist-pp ends here

;; [[file:chronometrist.org::*chronometrist-file][chronometrist-file:1]]
(defcustom chronometrist-file
  (locate-user-emacs-file "chronometrist")
  "Name (without extension) and full path of the Chronometrist database."
  :type 'file)

(defun chronometrist-file-variable-watcher (_symbol newval _operation _where)
  "Update slots of the active backend when `chronometrist-file' is changed.
For SYMBOL, NEWVAL, OPERATION, and WHERE, see `add-variable-watcher'."
  (chronometrist-on-file-path-change (chronometrist-active-backend) chronometrist-file newval))

(add-variable-watcher 'chronometrist-file #'chronometrist-file-variable-watcher)
;; chronometrist-file:1 ends here

;; [[file:chronometrist.org::*backend][backend:1]]
(defclass chronometrist-backend ()
  ((task-list :initform nil
              :initarg :task-list
              :accessor chronometrist-backend-task-list)))
;; backend:1 ends here

;; [[file:chronometrist.org::*backends-alist][backends-alist:1]]
(defvar chronometrist-backends-alist nil
  "Alist of Chronometrist backends.
Each element must be in the form `(KEYWORD TAG OBJECT)', where
TAG is a string used as a tag in customization, and OBJECT is an
EIEIO object such as one returned by `make-instance'.")
;; backends-alist:1 ends here

;; [[file:chronometrist.org::*active-backend][active-backend:1]]
(defcustom chronometrist-active-backend :plist
  "The backend currently in use.
Value must be a keyword corresponding to a key in
`chronometrist-backends-alist'."
  :type `(choice
          ,@(cl-loop for elt in chronometrist-backends-alist
              collect `(const :tag ,(cl-second elt) ,(cl-first elt)))))
;; active-backend:1 ends here

;; [[file:chronometrist.org::*active-backend][active-backend:1]]
(defun chronometrist-active-backend ()
  "Return an object representing the currently active backend."
  (cl-second (alist-get chronometrist-active-backend chronometrist-backends-alist)))
;; active-backend:1 ends here

;; [[file:chronometrist.org::*switch-backend][switch-backend:1]]
(defun chronometrist-switch-backend ()
  (interactive)
  (chronometrist-debug-message "[Command] switch-backend")
  (let* ((prompt (format "Switch to backend (current - %s): "
                         chronometrist-active-backend))
         (choice (chronometrist-read-backend-name prompt
                                     chronometrist-backends-alist
                                     (lambda (keyword)
                                       (not (eq chronometrist-active-backend
                                                keyword)))
                                     t)))
    (setq chronometrist-active-backend choice)
    (chronometrist-reset-backend (chronometrist-active-backend))
    ;; timer function is backend-dependent
    (chronometrist-force-restart-timer)))
;; switch-backend:1 ends here

;; [[file:chronometrist.org::*register-backend][register-backend:1]]
(defun chronometrist-register-backend (keyword tag object)
  "Add backend to `chronometrist-backends-alist'.
For values of KEYWORD, TAG, and OBJECT, see `chronometrist-backends-alist'.

If a backend with KEYWORD already exists, the existing entry will
be replaced."
  (setq chronometrist-backends-alist
        (assq-delete-all keyword chronometrist-backends-alist))
  (add-to-list 'chronometrist-backends-alist
               (list keyword tag object)
               t))
;; register-backend:1 ends here

;; [[file:chronometrist.org::*read-backend-name][read-backend-name:1]]
(defun chronometrist-read-backend-name (prompt backend-alist
                                  &optional predicate return-keyword)
  "Prompt user for a Chronometrist backend name.
BACKEND-ALIST should be an alist similar to `chronometrist-backends-alist'.

RETURN-KEYWORD, if non-nil, means return only the keyword of the
selected backend; otherwise, return the CLOS object for the
backend.

PROMPT and PREDICATE have the same meanings as in
`completing-read'."
  (let ((backend-keyword
         (read
          (completing-read prompt
                           (cl-loop for list in backend-alist
                             collect (cl-first list))
                           predicate t))))
    (if return-keyword
        backend-keyword
      (cl-second (alist-get backend-keyword backend-alist)))))
;; read-backend-name:1 ends here

;; [[file:chronometrist.org::*task-list][task-list:1]]
(defun chronometrist-task-list ()
  "Return the list of tasks to be used.
If `chronometrist-task-list' is non-nil, return its value; else,
return a list of tasks from the active backend."
  (let ((backend (chronometrist-active-backend)))
    (with-slots (task-list) backend
      (or chronometrist-task-list
          task-list
          (setf task-list (chronometrist-list-tasks backend))))))
;; task-list:1 ends here

;; [[file:chronometrist.org::*list-tasks][list-tasks:1]]
(defun chronometrist-list-tasks (backend)
  "Return all tasks recorded in BACKEND as a list of strings."
  (cl-loop for plist in (chronometrist-to-list backend)
    collect (plist-get plist :name) into names
    finally return
    (sort (cl-remove-duplicates names :test #'equal)
          #'string-lessp)))
;; list-tasks:1 ends here

;; [[file:chronometrist.org::*run-assertions][run-assertions:1]]
(cl-defgeneric chronometrist-backend-run-assertions (backend)
  "Check common preconditions for any operations on BACKEND.
Signal errors for any unmet preconditions.")
;; run-assertions:1 ends here

;; [[file:chronometrist.org::*view-backend][view-backend:1]]
(cl-defgeneric chronometrist-view-backend (backend)
  "Open BACKEND for interactive viewing.")
;; view-backend:1 ends here

;; [[file:chronometrist.org::*edit-backend][edit-backend:1]]
(cl-defgeneric chronometrist-edit-backend (backend)
  "Open BACKEND for interactive editing.")
;; edit-backend:1 ends here

;; [[file:chronometrist.org::*backend-empty-p][backend-empty-p:1]]
(cl-defgeneric chronometrist-backend-empty-p (backend)
  "Return non-nil if BACKEND contains no records, else nil.")
;; backend-empty-p:1 ends here

;; [[file:chronometrist.org::*backend-modified-p][backend-modified-p:1]]
(cl-defgeneric chronometrist-backend-modified-p (backend)
  "Return non-nil if BACKEND is being modified.
For instance, a file-based backend could be undergoing editing by
a user.")
;; backend-modified-p:1 ends here

;; [[file:chronometrist.org::*create-file][create-file:1]]
(cl-defgeneric chronometrist-create-file (backend &optional file)
  "Create file associated with BACKEND.
Use FILE as a path, if provided.
Return path of new file if successfully created, and nil if it already exists.")
;; create-file:1 ends here

;; [[file:chronometrist.org::*latest-date-records][latest-date-records:1]]
(cl-defgeneric chronometrist-latest-date-records (backend)
  "Return intervals of latest day in BACKEND as a tagged list (\"DATE\" PLIST*).
Return nil if BACKEND contains no records.")
;; latest-date-records:1 ends here

;; [[file:chronometrist.org::*insert][insert:1]]
(cl-defgeneric chronometrist-insert (backend plist)
  "Insert PLIST as new record in BACKEND.
Return non-nil if record is inserted successfully.")

(cl-defmethod chronometrist-insert :before ((_backend t) plist &key &allow-other-keys)
  (unless (cl-typep plist 'chronometrist-plist)
    (error "Not a valid plist: %S" plist)))
;; insert:1 ends here

;; [[file:chronometrist.org::*remove-last][remove-last:1]]
(cl-defgeneric chronometrist-remove-last (backend)
  "Remove last record from BACKEND.
Return non-nil if record is successfully removed.
Signal an error if there is no record to remove.")
;; remove-last:1 ends here

;; [[file:chronometrist.org::*latest-record][latest-record:1]]
(cl-defgeneric chronometrist-latest-record (backend)
  "Return the latest entry from BACKEND as a plist, or nil if BACKEND contains no records.
Return value may be active, i.e. it may or may not have a :stop key-value.")
;; latest-record:1 ends here

;; [[file:chronometrist.org::*task-records-for-date][task-records-for-date:1]]
(cl-defgeneric chronometrist-task-records-for-date (backend task date-ts)
  "From BACKEND, return records for TASK on DATE-TS as a list of plists.
DATE-TS must be a `ts.el' struct.

Return nil if BACKEND contains no records.")

(cl-defmethod chronometrist-task-records-for-date :before ((_backend t) task date-ts &key &allow-other-keys)
  (unless (cl-typep task 'string)
    (error "task %S is not a string" task))
  (unless (cl-typep date-ts 'ts)
    (error "date-ts %S is not a `ts' struct" date-ts)))
;; task-records-for-date:1 ends here

;; [[file:chronometrist.org::*replace-last][replace-last:1]]
(cl-defgeneric chronometrist-replace-last (backend plist)
  "Replace last record in BACKEND with PLIST.
Return non-nil if successful.")

(cl-defmethod chronometrist-replace-last :before ((_backend t) plist &key &allow-other-keys)
  (unless (cl-typep plist 'chronometrist-plist)
    (error "Not a valid plist: %S" plist)))
;; replace-last:1 ends here

;; [[file:chronometrist.org::*to-file][to-file:1]]
(cl-defgeneric chronometrist-to-file (input-hash-table output-backend output-file)
  "Save data from INPUT-HASH-TABLE to OUTPUT-FILE, in OUTPUT-BACKEND format.
Any existing data in OUTPUT-FILE is overwritten.")
;; to-file:1 ends here

;; [[file:chronometrist.org::*on-add][on-add:1]]
(cl-defgeneric chronometrist-on-add (backend)
  "Function called when data is added to BACKEND.
This may happen within Chronometrist (e.g. via
`chronometrist-insert') or outside it (e.g. a user editing the
backend file).

NEW-DATA is the data that was added.")
;; on-add:1 ends here

;; [[file:chronometrist.org::*on-modify][on-modify:1]]
(cl-defgeneric chronometrist-on-modify (backend)
  "Function called when data in BACKEND is modified (rather than added or removed).
This may happen within Chronometrist (e.g. via
`chronometrist-replace-last') or outside it (e.g. a user editing
the backend file).

OLD-DATA and NEW-DATA is the data before and after the changes,
respectively.")
;; on-modify:1 ends here

;; [[file:chronometrist.org::*on-remove][on-remove:1]]
(cl-defgeneric chronometrist-on-remove (backend)
  "Function called when data is removed from BACKEND.
This may happen within Chronometrist (e.g. via
`chronometrist-remove-last') or outside it (e.g. a user editing
the backend file).

OLD-DATA is the data that was modified.")
;; on-remove:1 ends here

;; [[file:chronometrist.org::*on-change][on-change:1]]
(cl-defgeneric chronometrist-on-change (backend)
  "Function to be run when BACKEND changes on disk.
This may happen within Chronometrist (e.g. via
`chronometrist-insert') or outside it (e.g. a user editing the
backend file).")
;; on-change:1 ends here

;; [[file:chronometrist.org::*verify][verify:1]]
(cl-defgeneric chronometrist-verify (backend)
  "Check BACKEND for errors in data.
Return nil if no errors are found.

If an error is found, return (LINE-NUMBER . COLUMN-NUMBER) for file-based backends.")
;; verify:1 ends here

;; [[file:chronometrist.org::*on-file-path-change][on-file-path-change:1]]
(cl-defgeneric chronometrist-on-file-path-change (backend old-path new-path)
  "Function run when the value of `chronometrist-file' is changed.
OLD-PATH and NEW-PATH are the old and new values of
`chronometrist-file', respectively.")
;; on-file-path-change:1 ends here

;; [[file:chronometrist.org::*reset-backend][reset-backend:1]]
(cl-defgeneric chronometrist-reset-backend (backend)
  "Reset data structures for BACKEND.")
;; reset-backend:1 ends here

;; [[file:chronometrist.org::*to-hash-table][to-hash-table:1]]
(cl-defgeneric chronometrist-to-hash-table (backend)
  "Return data in BACKEND as a hash table in chronological order.
Hash table keys are ISO-8601 date strings. Hash table values are
lists of records, represented by plists. Both hash table keys and
hash table values must be in chronological order.")
;; to-hash-table:1 ends here

;; [[file:chronometrist.org::*to-list][to-list:1]]
(cl-defgeneric chronometrist-to-list (backend)
  "Return all records in BACKEND as a list of plists.")
;; to-list:1 ends here

;; [[file:chronometrist.org::*memory-layer-empty-p][memory-layer-empty-p:1]]
(cl-defgeneric chronometrist-memory-layer-empty-p (backend)
  "Return non-nil if memory layer of BACKEND contains no records, else nil.")
;; memory-layer-empty-p:1 ends here

;; [[file:chronometrist.org::*active-days][active-days:1]]
(cl-defgeneric chronometrist-active-days (backend task &key start end)
  "From BACKEND, return number of days on which TASK had recorded time.")
;; active-days:1 ends here

;; [[file:chronometrist.org::*count-records][count-records:1]]
(cl-defgeneric chronometrist-count-records (backend)
  "Return number of records in BACKEND.")
;; count-records:1 ends here

;; [[file:chronometrist.org::#file-backend-mixin][file-backend-mixin:1]]
(defclass chronometrist-file-backend-mixin ()
  ((path :initform nil
    :initarg :path
    :accessor chronometrist-backend-path
    :custom 'string
    :documentation
    "Path to backend file, without extension.")
   (extension :initform nil
    :initarg :extension
    :accessor chronometrist-backend-ext
    :custom 'string
    :documentation
    "Extension of backend file.")
   (file :initarg :file
         :initform nil
         :accessor chronometrist-backend-file
         :custom 'string
         :documentation "Full path to backend file, with extension.")
   (hash-table :initform (chronometrist-make-hash-table)
               :initarg :hash-table
               :accessor chronometrist-backend-hash-table)
   (file-watch :initform nil
               :initarg :file-watch
               :accessor chronometrist-backend-file-watch
               :documentation "Filesystem watch object, as returned by `file-notify-add-watch'."))
  :documentation "Mixin for backends storing data in a single file.")
;; file-backend-mixin:1 ends here

;; [[file:chronometrist.org::*setup-file-watch][setup-file-watch:1]]
(cl-defun chronometrist-setup-file-watch (&optional (callback #'chronometrist-refresh-file))
  "Arrange for CALLBACK to be called when the backend file changes."
  (with-slots (file file-watch) (chronometrist-active-backend)
    (unless file-watch
      (setq file-watch
            (file-notify-add-watch file '(change) callback)))))
;; setup-file-watch:1 ends here

;; [[file:chronometrist.org::*edit-backend][edit-backend:1]]
(cl-defmethod chronometrist-edit-backend ((backend chronometrist-file-backend-mixin))
  (find-file-other-window (chronometrist-backend-file backend))
  (goto-char (point-max)))
;; edit-backend:1 ends here

;; [[file:chronometrist.org::*initialize-instance][initialize-instance:1]]
(cl-defmethod initialize-instance :after ((backend chronometrist-file-backend-mixin)
                                          &rest _initargs)
  "Initialize FILE based on PATH and EXTENSION."
  (with-slots (path extension file) backend
    (when (and path extension (not file))
      (setf file (concat path "." extension)))))
;; initialize-instance:1 ends here

;; [[file:chronometrist.org::*reset-backend][reset-backend:1]]
(cl-defmethod chronometrist-reset-backend ((backend chronometrist-file-backend-mixin))
  (with-slots (hash-table file-watch
               rest-start rest-end rest-hash
               file-length last-hash) backend
    (chronometrist-reset-task-list backend)
    (when file-watch
      (file-notify-rm-watch file-watch))
    (setf hash-table  (chronometrist-to-hash-table backend)
          file-watch  nil
          rest-start  nil
          rest-end    nil
          rest-hash   nil
          file-length nil
          last-hash   nil)
    (chronometrist-setup-file-watch)))
;; reset-backend:1 ends here

;; [[file:chronometrist.org::*backend-empty-p][backend-empty-p:1]]
(cl-defmethod chronometrist-backend-empty-p ((backend chronometrist-file-backend-mixin))
  (with-slots (file) backend
      (or (not (file-exists-p file))
          (chronometrist-file-empty-p file))))
;; backend-empty-p:1 ends here

;; [[file:chronometrist.org::*memory-layer-empty-p][memory-layer-empty-p:1]]
(cl-defmethod chronometrist-memory-layer-empty-p ((backend chronometrist-file-backend-mixin))
  (with-slots (hash-table) backend
    (zerop (hash-table-count hash-table))))
;; memory-layer-empty-p:1 ends here

;; [[file:chronometrist.org::*backend-modified-p][backend-modified-p:1]]
(cl-defmethod chronometrist-backend-modified-p ((backend chronometrist-file-backend-mixin))
  (with-slots (file) backend
    (buffer-modified-p
     (get-buffer-create
      (find-file-noselect file)))))
;; backend-modified-p:1 ends here

;; [[file:chronometrist.org::*on-file-path-change][on-file-path-change:1]]
(cl-defmethod chronometrist-on-file-path-change ((backend chronometrist-file-backend-mixin) _old-path new-path)
  (with-slots (path extension file) backend
    (setf path new-path
          file (concat path "." extension))))
;; on-file-path-change:1 ends here

;; [[file:chronometrist.org::*elisp-sexp-backend][elisp-sexp-backend:1]]
(defclass chronometrist-elisp-sexp-backend (chronometrist-backend chronometrist-file-backend-mixin)
  ((rest-start :initarg :rest-start
               :initform nil
               :accessor chronometrist-backend-rest-start
               :documentation "Integer denoting start of first s-expression in file.")
   (rest-end :initarg :rest-end
             :initform nil
             :accessor chronometrist-backend-rest-end
             :documentation "Integer denoting end of second-last s-expression in file.")
   (rest-hash :initarg :rest-hash
              :initform nil
              :accessor chronometrist-backend-rest-hash
              :documentation "Hash of content between rest-start and rest-end.")
   (file-length :initarg :file-length
                :initform nil
                :accessor chronometrist-backend-file-length
                :documentation "Integer denoting length of file, as returned by `(point-max)'.")
   (last-hash :initarg :last-hash
              :initform nil
              :accessor chronometrist-backend-last-hash
              :documentation "Hash of content between rest-end and file-length."))
  :documentation "Base class for any text file backend which stores s-expressions readable by Emacs Lisp.")
;; elisp-sexp-backend:1 ends here

;; [[file:chronometrist.org::*sexp-mode][sexp-mode:1]]
(define-derived-mode chronometrist-sexp-mode
  ;; fundamental-mode
  emacs-lisp-mode
  "chronometrist-sexp")
;; sexp-mode:1 ends here

;; [[file:chronometrist.org::*create-file][create-file:1]]
(cl-defmethod chronometrist-create-file ((backend chronometrist-elisp-sexp-backend) &optional file)
  (let ((file (or file (chronometrist-backend-file backend))))
    (unless (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (erase-buffer)
        (goto-char (point-min))
        (insert ";;; -*- mode: chronometrist-sexp; -*-\n\n")
        (write-file file))
      file)))
;; create-file:1 ends here

;; [[file:chronometrist.org::*in-file][in-file:1]]
(defmacro chronometrist-sexp-in-file (file &rest body)
  "Run BODY in a buffer visiting FILE, restoring point afterwards."
  (declare (indent defun) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion ,@body)))
;; in-file:1 ends here

;; [[file:chronometrist.org::*pre-read-check][pre-read-check:1]]
(defun chronometrist-sexp-pre-read-check (buffer)
  "Return non-nil if there is an s-expression before point in BUFFER.
Move point to the start of this s-expression."
  (with-current-buffer buffer
    (and (not (bobp))
         (backward-list)
         (or (not (bobp))
             (not (looking-at-p "^[[:blank:]]*;"))))))
;; pre-read-check:1 ends here

;; [[file:chronometrist.org::*loop-sexp-file][loop-sexp-file:1]]
(defmacro chronometrist-loop-sexp-file (_for sexp _in file &rest loop-clauses)
  "`cl-loop' LOOP-CLAUSES over s-expressions in FILE.
SEXP is bound to each s-expressions in reverse order (last
expression first)."
  (declare (indent defun) (debug 'cl-loop))
  `(chronometrist-sexp-in-file ,file
     (goto-char (point-max))
     (cl-loop with ,sexp
       while (and (chronometrist-sexp-pre-read-check (current-buffer))
                  (setq ,sexp (ignore-errors (read (current-buffer))))
                  (backward-list))
       ,@loop-clauses)))
;; loop-sexp-file:1 ends here

;; [[file:chronometrist.org::*backend-empty-p][backend-empty-p:1]]
(cl-defmethod chronometrist-backend-empty-p ((backend chronometrist-elisp-sexp-backend))
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-min))
    (not (ignore-errors
           (read (current-buffer))))))
;; backend-empty-p:1 ends here

;; [[file:chronometrist.org::*indices and hashes][indices and hashes:1]]
(defun chronometrist-rest-start (file)
  (chronometrist-sexp-in-file file
    (goto-char (point-min))
    (forward-list)
    (backward-list)
    (point)))

(defun chronometrist-rest-end (file)
  (chronometrist-sexp-in-file file
    (goto-char (point-max))
    (backward-list 2)
    (forward-list)
    (point)))

(defun chronometrist-file-length (file)
  (chronometrist-sexp-in-file file (point-max)))
;; indices and hashes:1 ends here

;; [[file:chronometrist.org::*file-hash][file-hash:1]]
(cl-defun chronometrist-file-hash (start end &optional (file (chronometrist-backend-file (chronometrist-active-backend))))
  "Calculate hash of `chronometrist-file' between START and END."
  (chronometrist-sexp-in-file file
    (secure-hash 'sha1
                 (buffer-substring-no-properties start end))))
;; file-hash:1 ends here

;; [[file:chronometrist.org::*file-change-type][file-change-type:1]]
(defun chronometrist-file-change-type (backend)
  "Determine the type of change made to BACKEND's file.
    Return
    :append  if a new s-expression was added to the end,
    :modify  if the last s-expression was modified,
    :remove  if the last s-expression was removed,
        nil  if the contents didn't change, and
          t  for any other change."
  (with-slots
      (file file-watch
            ;; The slots contain the old state of the file.
            hash-table
            rest-start rest-end rest-hash
            file-length last-hash) backend
    (let* ((new-length    (chronometrist-file-length file))
           (new-rest-hash (when (and (>= new-length rest-start)
                                     (>= new-length rest-end))
                            (chronometrist-file-hash rest-start rest-end file)))
           (new-last-hash (when (and (>= new-length rest-end)
                                     (>= new-length file-length))
                            (chronometrist-file-hash rest-end file-length file))))
      ;; (chronometrist-debug-message "File indices - old rest-start: %s rest-end: %s file-length: %s new-length: %s"
      ;;          rest-start rest-end file-length new-length)
      (cond ((and (= file-length new-length)
                  (equal rest-hash new-rest-hash)
                  (equal last-hash new-last-hash))
             nil)
            ((or (< new-length rest-end) ;; File has shrunk so much that we cannot compare rest-hash.
                 (not (equal rest-hash new-rest-hash)))
             t)
            ;; From here on, it is implicit that the change has happened at the end of the file.
            ((and (< file-length new-length) ;; File has grown.
                  (equal last-hash new-last-hash))
             :append)
            ((and (< new-length file-length) ;; File has shrunk.
                  (not (chronometrist-sexp-in-file file
                         (goto-char rest-end)
                         (ignore-errors
                           (read (current-buffer)))))) ;; There is no sexp after rest-end.
             :remove)
            (t :modify)))))
;; file-change-type:1 ends here

;; [[file:chronometrist.org::*reset-task-list][reset-task-list:1]]
(cl-defun chronometrist-reset-task-list (backend)
  "Regenerate BACKEND's task list from its data.
Only takes effect if `chronometrist-task-list' is nil (i.e. the
user has not defined their own task list)."
  (unless chronometrist-task-list
    (setf (chronometrist-backend-task-list backend) (chronometrist-list-tasks backend))))
;; reset-task-list:1 ends here

;; [[file:chronometrist.org::*add-to-task-list][add-to-task-list:1]]
(defun chronometrist-add-to-task-list (task backend)
  "Add TASK to BACKEND's task list, if it is not already present.
Only takes effect if `chronometrist-task-list' is nil (i.e. the
user has not defined their own task list)."
  (with-slots (task-list) backend
    (unless (and (not chronometrist-task-list)
                 (cl-member task task-list :test #'equal))
      (setf task-list
            (sort (cons task task-list)
                  #'string-lessp)))))
;; add-to-task-list:1 ends here

;; [[file:chronometrist.org::*remove-from-task-list][remove-from-task-list:1]]
(defun chronometrist-remove-from-task-list (task backend)
  "Remove TASK from BACKEND's task list if necessary.
TASK is removed if it does not occur in BACKEND's hash table, or
if it only occurs in the newest plist of the same.

Only takes effect if `chronometrist-task-list' is nil (i.e. the
user has not defined their own task list).

Return new value of BACKEND's task list, or nil if
unchanged."
  (with-slots (hash-table task-list) backend
    (unless chronometrist-task-list
      (let (;; number of plists in hash table
            (ht-plist-count (cl-loop with count = 0
                              for intervals being the hash-values of hash-table
                              do (cl-loop for _interval in intervals
                                   do (cl-incf count))
                              finally return count))
            ;; index of first occurrence of TASK in hash table, or nil if not found
            (ht-task-first-result (cl-loop with count = 0
                                    for intervals being the hash-values of hash-table
                                    when (cl-loop for interval in intervals
                                           do (cl-incf count)
                                           when (equal task (plist-get interval :name))
                                           return t)
                                    return count)))
        (when (or (not ht-task-first-result)
                  (= ht-task-first-result ht-plist-count))
          ;; The only interval for TASK is the last expression
          (setf task-list (remove task task-list)))))))
;; remove-from-task-list:1 ends here

;; [[file:chronometrist.org::*on-change][on-change:1]]
(cl-defmethod chronometrist-on-change ((backend chronometrist-elisp-sexp-backend) fs-event)
  "Function called when BACKEND file is changed.
This may happen within Chronometrist (through the backend
protocol) or outside it (e.g. a user editing the backend file).

FS-EVENT is the event passed by the `filenotify' library (see `file-notify-add-watch')."
  (with-slots (file hash-table file-watch
                    rest-start rest-end rest-hash
                    file-length last-hash) backend
    (-let* (((_ action _ _) fs-event)
            (file-state-bound-p (and rest-start rest-end rest-hash
                                     file-length last-hash))
            (change      (when file-state-bound-p
                           (chronometrist-file-change-type backend)))
            (reset-watch-p (or (eq action 'deleted)
                               (eq action 'renamed))))
      (chronometrist-debug-message "[Method] on-change: file change type %s" change)
      ;; If only the last plist was changed, update hash table and
      ;; task list, otherwise clear and repopulate hash table.
      (cond ((or reset-watch-p
                 (not file-state-bound-p) ;; why?
                 (eq change t))
             (chronometrist-reset-backend backend))
            (file-state-bound-p
             (pcase change
               ;; A new s-expression was added at the end of the file
               (:append (chronometrist-on-add backend))
               ;; The last s-expression in the file was changed
               (:modify (chronometrist-on-modify backend))
               ;; The last s-expression in the file was removed
               (:remove (chronometrist-on-remove backend))
               ((pred null) nil))))
      (setf rest-start  (chronometrist-rest-start file)
            rest-end    (chronometrist-rest-end file)
            file-length (chronometrist-file-length file)
            last-hash   (chronometrist-file-hash rest-end file-length file)
            rest-hash   (chronometrist-file-hash rest-start rest-end file)))))
;; on-change:1 ends here

;; [[file:chronometrist.org::*backend][backend:1]]
(defclass chronometrist-plist-backend (chronometrist-elisp-sexp-backend)
  ((extension :initform "plist"
              :accessor chronometrist-backend-ext
              :custom 'string)))

(chronometrist-register-backend
 :plist "Store records as plists."
 (make-instance 'chronometrist-plist-backend :path chronometrist-file))
;; backend:1 ends here

;; [[file:chronometrist.org::*pretty-print-function][pretty-print-function:1]]
(defcustom chronometrist-sexp-pretty-print-function #'chronometrist-plist-pp
  "Function used to pretty print plists in `chronometrist-file'.
Like `pp', it must accept an OBJECT and optionally a
STREAM (which is the value of `current-buffer')."
  :type 'function
  :group 'chronometrist)
;; pretty-print-function:1 ends here

;; [[file:chronometrist.org::*latest-date-records][latest-date-records:1]]
(cl-defmethod chronometrist-latest-date-records ((backend chronometrist-plist-backend))
  (chronometrist-backend-run-assertions backend)
  (with-slots (hash-table) backend
    (when-let*
        ((latest-date (chronometrist-events-last-date hash-table))
         (records     (gethash latest-date hash-table)))
      (cons latest-date records))))
;; latest-date-records:1 ends here

;; [[file:chronometrist.org::*to-hash-table][to-hash-table:1]]
(cl-defmethod chronometrist-to-hash-table ((backend chronometrist-plist-backend))
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-min))
    (let ((table (chronometrist-make-hash-table))
          expr pending-expr)
      (while (or pending-expr
                 (setq expr (ignore-errors (read (current-buffer)))))
        ;; find and split midnight-spanning events during deserialization itself
        (let* ((split-expr (chronometrist-split-plist expr))
               (new-value  (cond (pending-expr
                                  (prog1 pending-expr
                                    (setq pending-expr nil)))
                                 (split-expr
                                  (setq pending-expr (cl-second split-expr))
                                  (cl-first split-expr))
                                 (t expr)))
               (new-value-date (--> (plist-get new-value :start)
                                    (substring it 0 10)))
               (existing-value (gethash new-value-date table)))
          (puthash new-value-date
                   (if existing-value
                       (append existing-value
                               (list new-value))
                     (list new-value))
                   table)))
      table)))
;; to-hash-table:1 ends here

;; [[file:chronometrist.org::*insert][insert:1]]
(cl-defmethod chronometrist-insert ((backend chronometrist-plist-backend) plist &key (save t))
  (chronometrist-backend-run-assertions backend)
  (chronometrist-debug-message "[Method] insert: %s" plist)
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-max))
    ;; If we're adding the first s-exp in the file, don't add a
    ;; newline before it
    (unless (bobp) (insert "\n"))
    (unless (bolp) (insert "\n"))
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (when save (save-buffer))
    t))
;; insert:1 ends here

;; [[file:chronometrist.org::*remove-last][remove-last:1]]
(cl-defmethod chronometrist-remove-last ((backend chronometrist-plist-backend))
  (chronometrist-debug-message "[Method] remove-last")
  (chronometrist-backend-run-assertions backend)
  (when (chronometrist-backend-empty-p backend)
  (error "chronometrist-remove-last has nothing to remove in %s"
         (eieio-object-class-name backend)))
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-max))
    ;; this condition should never really occur, since we insert a
    ;; file local variable prop line when the file is created...
    (unless (and (bobp) (bolp)) (insert "\n"))
    (backward-list 1)
    (chronometrist-sexp-delete-list)))
;; remove-last:1 ends here

;; [[file:chronometrist.org::*reindent-buffer][reindent-buffer:1]]
(defun chronometrist-sexp-reindent-buffer ()
  "Reindent the current buffer.
This is meant to be run in `chronometrist-file' when using an s-expression backend."
  (interactive)
  (chronometrist-debug-message "[Command] reindent-buffer")
  (let (expr)
    (goto-char (point-min))
    (while (setq expr (ignore-errors (read (current-buffer))))
      (backward-list)
      (chronometrist-sexp-delete-list)
      (when (looking-at "\n*")
        (delete-region (match-beginning 0) (match-end 0)))
      (funcall chronometrist-sexp-pretty-print-function expr (current-buffer))
      (insert "\n")
      (unless (eobp) (insert "\n")))))
;; reindent-buffer:1 ends here

;; [[file:chronometrist.org::*to-file][to-file:1]]
(cl-defmethod chronometrist-to-file (hash-table (backend chronometrist-plist-backend) file)
  (delete-file file)
  (chronometrist-create-file backend file)
  (chronometrist-reset-backend backend)             ; possibly to ensure BACKEND is up to date
  (chronometrist-sexp-in-file file
    (goto-char (point-max))
    (cl-loop
      for date in (sort (hash-table-keys hash-table) #'string-lessp) do
      (cl-loop for plist in (gethash date hash-table) do
        (insert (chronometrist-plist-pp plist) "\n\n"))
      finally do (save-buffer))))
;; to-file:1 ends here

;; [[file:chronometrist.org::*to-list][to-list:1]]
(cl-defmethod chronometrist-to-list ((backend chronometrist-plist-backend))
  (chronometrist-backend-run-assertions backend)
  (chronometrist-loop-sexp-file for expr in (chronometrist-backend-file backend) collect expr))
;; to-list:1 ends here

;; [[file:chronometrist.org::*on-add][on-add:1]]
(cl-defmethod chronometrist-on-add ((backend chronometrist-plist-backend))
  "Function run when a new plist is added at the end of a
`chronometrist-plist-backend' file."
  (with-slots (hash-table) backend
    (-let [(new-plist &as &plist :name new-task) (chronometrist-latest-record backend)]
      (setf hash-table (chronometrist-events-update new-plist hash-table))
      (chronometrist-add-to-task-list new-task backend))))
;; on-add:1 ends here

;; [[file:chronometrist.org::*on-modify][on-modify:1]]
(cl-defmethod chronometrist-on-modify ((backend chronometrist-plist-backend))
  "Function run when the newest plist in a
`chronometrist-plist-backend' file is modified."
  (with-slots (hash-table) backend
    (-let (((new-plist &as &plist :name new-task) (chronometrist-latest-record backend))
           ((&plist :name old-task) (chronometrist-events-last backend)))
      (setf hash-table (chronometrist-events-update new-plist hash-table t))
      (chronometrist-remove-from-task-list old-task backend)
      (chronometrist-add-to-task-list new-task backend))))
;; on-modify:1 ends here

;; [[file:chronometrist.org::*on-remove][on-remove:1]]
(cl-defmethod chronometrist-on-remove ((backend chronometrist-plist-backend))
  "Function run when the newest plist in a
`chronometrist-plist-backend' file is deleted."
  (with-slots (hash-table) backend
    (-let (((&plist :name old-task) (chronometrist-events-last))
           (date  (chronometrist-events-last-date hash-table)))
      ;; `chronometrist-remove-from-task-list' checks the hash table to determine
      ;; if `chronometrist-task-list' is to be updated. Thus, the hash table must
      ;; not be updated until the task list is.
      (chronometrist-remove-from-task-list old-task backend)
      (--> (gethash date hash-table)
           (-drop-last 1 it)
           (setf (gethash date hash-table) it)))))
;; on-remove:1 ends here

;; [[file:chronometrist.org::#program-backend-plist-latest-record][latest-record:1]]
(cl-defmethod chronometrist-latest-record ((backend chronometrist-plist-backend))
  (chronometrist-backend-run-assertions backend)
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-max))
    (backward-list)
    (ignore-errors (read (current-buffer)))))
;; latest-record:1 ends here

;; [[file:chronometrist.org::*task-records-for-date][task-records-for-date:1]]
(cl-defmethod chronometrist-task-records-for-date ((backend chronometrist-plist-backend) task date-ts)
  (chronometrist-backend-run-assertions backend)
  (let* ((date    (chronometrist-date-iso date-ts))
         (records (gethash date (chronometrist-backend-hash-table backend))))
    (cl-loop for record in records
      when (equal task (plist-get record :name))
      collect record)))
;; task-records-for-date:1 ends here

;; [[file:chronometrist.org::*replace-last][replace-last:1]]
(cl-defmethod chronometrist-replace-last ((backend chronometrist-plist-backend) plist)
  (chronometrist-debug-message "[Method] replace-last with %s" plist)
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (chronometrist-remove-last backend))
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (save-buffer)
    t))
;; replace-last:1 ends here

;; [[file:chronometrist.org::*count-records][count-records:1]]
(cl-defmethod chronometrist-count-records ((backend chronometrist-plist-backend))
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-min))
    (cl-loop with count = 0
      while (ignore-errors (read (current-buffer)))
      do (cl-incf count)
      finally return count)))
;; count-records:1 ends here

;; [[file:chronometrist.org::*backend][backend:1]]
(defclass chronometrist-plist-group-backend (chronometrist-elisp-sexp-backend)
  ((extension :initform "plg"
              :accessor chronometrist-backend-ext
              :custom 'string)))

(chronometrist-register-backend
 :plist-group "Store records as plists grouped by date."
 (make-instance 'chronometrist-plist-group-backend
                :path chronometrist-file))
;; backend:1 ends here

;; [[file:chronometrist.org::*backward-read-sexp][backward-read-sexp:1]]
(defun chronometrist-backward-read-sexp (buffer)
  (backward-list)
  (save-excursion (read buffer)))
;; backward-read-sexp:1 ends here

;; [[file:chronometrist.org::*run-assertions][run-assertions:1]]
(cl-defmethod chronometrist-backend-run-assertions ((backend chronometrist-file-backend-mixin))
  (with-slots (file) backend
    (unless (file-exists-p file)
      (error "Backend file %S does not exist" file))))
;; run-assertions:1 ends here

;; [[file:chronometrist.org::*latest-date-records][latest-date-records:1]]
(cl-defmethod chronometrist-latest-date-records ((backend chronometrist-plist-group-backend))
  (chronometrist-backend-run-assertions backend)
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (goto-char (point-max))
    (ignore-errors
      (chronometrist-backward-read-sexp (current-buffer)))))
;; latest-date-records:1 ends here

;; [[file:chronometrist.org::*insert][insert:1]]
(cl-defmethod chronometrist-insert ((backend chronometrist-plist-group-backend) plist &key (save t))
  (cl-check-type plist chronometrist-plist)
  (chronometrist-debug-message "[Method] insert: %S" plist)
  (chronometrist-backend-run-assertions backend)
  (if (not plist)
      (error "%s" "`chronometrist-insert' was called with an empty plist")
    (chronometrist-sexp-in-file (chronometrist-backend-file backend)
      (-let* (((plist-1 plist-2)   (chronometrist-split-plist plist))
              ;; Determine if we need to insert a new plist group
              (latest-plist-group  (chronometrist-latest-date-records backend))
              (backend-latest-date (cl-first latest-plist-group))
              (date-today          (chronometrist-date-iso))
              (insert-new-group    (not (equal date-today backend-latest-date)))
              (start-date          (cl-first (split-string (plist-get plist :start) "T")))
              (new-plist-group-1   (if latest-plist-group
                                       (append latest-plist-group
                                               (list (or plist-1 plist)))
                                     (list start-date (or plist-1 plist))))
              (new-plist-group-2   (when (or plist-2 insert-new-group)
                                     (list date-today (or plist-2 plist)))))
        (goto-char (point-max))
        (when (not latest-plist-group)
          ;; first record
          (while (forward-comment 1) nil))
        (if (and plist-1 plist-2)
            ;; inactive, day-crossing record
            (progn
              (when latest-plist-group
                ;; not the first record
                (chronometrist-sexp-pre-read-check (current-buffer))
                (chronometrist-sexp-delete-list))
              (funcall chronometrist-sexp-pretty-print-function new-plist-group-1 (current-buffer))
              (dotimes (_ 2) (default-indent-new-line))
              (funcall chronometrist-sexp-pretty-print-function new-plist-group-2 (current-buffer)))
          ;; active, or non-day-crossing inactive record
          ;; insert into new group
          (if (or (not latest-plist-group) ;; first record
                  insert-new-group)
              (progn
                (default-indent-new-line)
                (funcall chronometrist-sexp-pretty-print-function new-plist-group-2 (current-buffer)))
            ;; insert into existing group
            (chronometrist-sexp-pre-read-check (current-buffer))
            (chronometrist-sexp-delete-list)
            (funcall chronometrist-sexp-pretty-print-function new-plist-group-1 (current-buffer))))
        (when save (save-buffer))
        t))))
;; insert:1 ends here

;; [[file:chronometrist.org::*plists-split-p][plists-split-p:1]]
(defun chronometrist-plists-split-p (old-plist new-plist)
  "Return t if OLD-PLIST and NEW-PLIST are split plists.
Split plists means the :stop time of old-plist must be the same as
the :start time of new-plist, and they must have identical
keyword-values (except :start and :stop)."
  (-let* (((&plist :stop  old-stop)  old-plist)
          ((&plist :start new-start) new-plist)
          (old-stop-unix     (parse-iso8601-time-string old-stop))
          (new-start-unix    (parse-iso8601-time-string new-start))
          (old-plist-no-time (chronometrist-plist-remove old-plist :start :stop))
          (new-plist-no-time (chronometrist-plist-remove new-plist :start :stop)))
    (and (time-equal-p old-stop-unix
                       new-start-unix)
         (equal old-plist-no-time
                new-plist-no-time))))
;; plists-split-p:1 ends here

;; [[file:chronometrist.org::*last-two-split-p][last-two-split-p:1]]
(defun chronometrist-last-two-split-p (file)
  "Return non-nil if the latest two plists in FILE are split.
FILE must be a file containing plist groups, as created by
`chronometrist-plist-backend'.

Return value is either a list in the form
(OLDER-PLIST NEWER-PLIST), or nil."
  (chronometrist-sexp-in-file file
    (let* ((newer-group (progn (goto-char (point-max))
                               (backward-list)
                               (read (current-buffer))))
           (older-group (and (= 2 (length newer-group))
                             (backward-list 2)
                             (read (current-buffer))))
           ;; in case there was just one plist-group in the file
           (older-group (unless (equal older-group newer-group)
                          older-group))
           (newer-plist (cl-second newer-group))
           (older-plist (cl-first (last older-group))))
      (when (and older-plist newer-plist
                 (chronometrist-plists-split-p older-plist newer-plist))
        (list older-plist newer-plist)))))
;; last-two-split-p:1 ends here

;; [[file:chronometrist.org::*plist-unify][plist-unify:1]]
(defun chronometrist-plist-unify (old-plist new-plist)
  "Return a plist with the :start of OLD-PLIST and the :stop of NEW-PLIST."
  (let ((old-plist-wo-time (chronometrist-plist-remove old-plist :start :stop))
        (new-plist-wo-time (chronometrist-plist-remove new-plist :start :stop)))
    (cond ((not (and old-plist new-plist)) nil)
          ((equal old-plist-wo-time new-plist-wo-time)
           (let ((plist (cl-copy-list old-plist)))
             (plist-put plist :stop (plist-get new-plist :stop))))
          (t (error "Attempt to unify plists with non-identical key-values")))))
;; plist-unify:1 ends here

;; [[file:chronometrist.org::*remove-last][remove-last:1]]
(cl-defmethod chronometrist-remove-last ((backend chronometrist-plist-group-backend) &key (save t))
  (with-slots (file) backend
    (chronometrist-sexp-in-file file
      (goto-char (point-max))
      (when (chronometrist-backend-empty-p backend)
        (error "chronometrist-remove-last has nothing to remove in %s"
               (eieio-object-class-name backend)))
      (when (chronometrist-last-two-split-p file) ;; cannot be checked after changing the file
        ;; latest plist-group has only one plist, which is split - delete the group
        (backward-list)
        (chronometrist-sexp-delete-list))
      ;; remove the last plist in the last plist-group
      ;; if the plist-group has only one plist, delete the group
      (let ((plist-group (save-excursion (backward-list)
                                         (read (current-buffer)))))
        (if (= 2 (length plist-group))
            (progn (backward-list)
                   (chronometrist-sexp-delete-list))
          (down-list -1)
          (backward-list)
          (chronometrist-sexp-delete-list)
          (join-line))
        (when save (save-buffer))
        t))))
;; remove-last:1 ends here

;; [[file:chronometrist.org::*to-list][to-list:1]]
(cl-defmethod chronometrist-to-list ((backend chronometrist-plist-group-backend))
  (chronometrist-backend-run-assertions backend)
  (chronometrist-loop-sexp-file for expr in (chronometrist-backend-file backend)
    append (reverse (cl-rest expr))))
;; to-list:1 ends here

;; [[file:chronometrist.org::*to-hash-table][to-hash-table:1]]
(cl-defmethod chronometrist-to-hash-table ((backend chronometrist-plist-group-backend))
  (with-slots (file) backend
    (chronometrist-loop-sexp-file for plist-group in file
      with table = (chronometrist-make-hash-table) do
      (puthash (cl-first plist-group) (cl-rest plist-group) table)
      finally return table)))
;; to-hash-table:1 ends here

;; [[file:chronometrist.org::*to-file][to-file:1]]
(cl-defmethod chronometrist-to-file (hash-table (backend chronometrist-plist-group-backend) file)
  (cl-check-type hash-table hash-table)
  (delete-file file)
  (chronometrist-create-file backend file)
  (chronometrist-reset-backend backend)
  (chronometrist-sexp-in-file file
    (goto-char (point-max))
    (cl-loop for date being the hash-keys of hash-table
      using (hash-values plists) do
      (insert
       (chronometrist-plist-pp (apply #'list date plists))
       "\n")
      finally do (save-buffer))))
;; to-file:1 ends here

;; [[file:chronometrist.org::*on-add][on-add:1]]
(cl-defmethod chronometrist-on-add ((backend chronometrist-plist-group-backend))
  "Function run when a new plist-group is added at the end of a
`chronometrist-plist-group-backend' file."
  (with-slots (hash-table) backend
    (-let [(date plist) (chronometrist-latest-date-records backend)]
      (puthash date plist hash-table)
      (chronometrist-add-to-task-list (plist-get plist :name) backend))))
;; on-add:1 ends here

;; [[file:chronometrist.org::*on-modify][on-modify:1]]
(cl-defmethod chronometrist-on-modify ((backend chronometrist-plist-group-backend))
  "Function run when the newest plist-group in a
`chronometrist-plist-group-backend' file is modified."
  (with-slots (hash-table) backend
    (-let* (((date . plists) (chronometrist-latest-date-records backend))
            (old-date        (chronometrist-events-last-date hash-table))
            (old-plists      (gethash old-date hash-table)))
      (puthash date plists hash-table)
      (cl-loop for plist in old-plists
        do (chronometrist-remove-from-task-list (plist-get plist :name) backend))
      (cl-loop for plist in plists
        do (chronometrist-add-to-task-list (plist-get plist :name) backend)))))
;; on-modify:1 ends here

;; [[file:chronometrist.org::*on-remove][on-remove:1]]
(cl-defmethod chronometrist-on-remove ((backend chronometrist-plist-group-backend))
  "Function run when the newest plist-group in a
`chronometrist-plist-group-backend' file is deleted."
  (with-slots (hash-table) backend
    (-let* ((old-date        (chronometrist-events-last-date hash-table))
            (old-plists      (gethash old-date hash-table)))
      (cl-loop for plist in old-plists
        do (chronometrist-remove-from-task-list (plist-get plist :name) backend))
      (puthash old-date nil hash-table))))
;; on-remove:1 ends here

;; [[file:chronometrist.org::*verify][verify:1]]
(cl-defmethod chronometrist-verify ((backend chronometrist-plist-group-backend))
  (with-slots (file hash-table) backend
    ;; incorrectly ordered groups check
    (chronometrist-loop-sexp-file for group in file
      with old-date-iso with old-date-unix
      with new-date-iso with new-date-unix
      ;; while (not (bobp))
      do (setq new-date-iso  (cl-first group)
               new-date-unix (parse-iso8601-time-string new-date-iso))
      when (and old-date-unix
                (time-less-p old-date-unix
                             new-date-unix))
      do (cl-return (format "%s appears before %s on line %s"
                            new-date-iso old-date-iso (line-number-at-pos)))
      else do (setq old-date-iso  new-date-iso
                    old-date-unix new-date-unix)
      finally return "Yay, no errors! (...that I could find 💀)")))
;; verify:1 ends here

;; [[file:chronometrist.org::*latest-record][latest-record:1]]
(cl-defmethod chronometrist-latest-record ((backend chronometrist-plist-group-backend))
  (cl-first (last (chronometrist-latest-date-records backend))))
;; latest-record:1 ends here

;; [[file:chronometrist.org::*task-records-for-date][task-records-for-date:1]]
(cl-defmethod chronometrist-task-records-for-date ((backend chronometrist-plist-group-backend)
                                      task date-ts)
  (cl-check-type task string)
  (cl-check-type date-ts ts)
  (chronometrist-backend-run-assertions backend)
  (cl-loop for plist in (gethash (chronometrist-date-iso date-ts)
                                 (chronometrist-backend-hash-table backend))
    when (equal task (plist-get plist :name))
    collect plist))
;; task-records-for-date:1 ends here

;; [[file:chronometrist.org::*replace-last][replace-last:1]]
(cl-defmethod chronometrist-replace-last ((backend chronometrist-plist-group-backend) plist)
  (cl-check-type plist chronometrist-plist)
  (when (chronometrist-backend-empty-p backend)
    (error "No record to replace in %s" (eieio-object-class-name backend)))
  (chronometrist-sexp-in-file (chronometrist-backend-file backend)
    (chronometrist-remove-last backend :save nil)
    (delete-trailing-whitespace)
    (chronometrist-insert backend plist :save nil)
    (save-buffer)
    t))
;; replace-last:1 ends here

;; [[file:chronometrist.org::*remove-prefix][remove-prefix:1]]
(defun chronometrist-remove-prefix (string)
  (replace-regexp-in-string "^chronometrist-" "" string))
;; remove-prefix:1 ends here

;; [[file:chronometrist.org::*migrate][migrate:1]]
(defun chronometrist-migrate ()
  "Convert from one Chronometrist backend to another."
  (interactive)
  (let* ((input-backend
          (chronometrist-read-backend-name "Backend to convert: "
                              chronometrist-backends-alist))
         (input-file-suggestion (chronometrist-backend-file input-backend))
         (input-file (read-file-name "File to convert: " nil
                                     input-file-suggestion t
                                     input-file-suggestion))
         (output-backend (chronometrist-read-backend-name
                          "Backend to write: "
                          chronometrist-backends-alist
                          (lambda (keyword)
                            (not (equal (cl-second
                                         (alist-get keyword chronometrist-backends-alist))
                                        input-backend)))))
         (output-file-suggestion (chronometrist-backend-file output-backend))
         (output-file (read-file-name "File to write: " nil nil nil
                                      output-file-suggestion))
         (input-backend-name  (chronometrist-remove-prefix
                               (symbol-name
                                (eieio-object-class-name input-backend))))
         (output-backend-name (chronometrist-remove-prefix
                               (symbol-name
                                (eieio-object-class-name output-backend))))
         (confirm (yes-or-no-p
                   (format "Convert %s (%s) to %s (%s)? "
                           input-file
                           input-backend-name
                           output-file
                           output-backend-name)))
         (confirm-exists
          (if (and confirm
                   (file-exists-p output-file)
                   (not (chronometrist-file-empty-p output-file)))
              (yes-or-no-p
               (format "Overwrite existing non-empty file %s ?"
                       output-file))
            t)))
    (if (and confirm confirm-exists)
        (chronometrist-to-file (chronometrist-backend-hash-table input-backend)
                  output-backend
                  output-file)
      (message "Conversion aborted."))))
;; migrate:1 ends here

;; [[file:chronometrist.org::*table][table:1]]
(defvar chronometrist-migrate-table (make-hash-table))
;; table:1 ends here

;; [[file:chronometrist.org::*populate][populate:1]]
(defun chronometrist-migrate-populate (in-file)
  "Read data from IN-FILE to `chronometrist-migrate-table'.
IN-FILE should be a file in the format supported by timeclock.el.
See `timeclock-log-data' for a description."
  (clrhash chronometrist-migrate-table)
  (with-current-buffer (find-file-noselect in-file)
    (save-excursion
      (goto-char (point-min))
      (let ((key-counter 0))
        (while (not (eobp))
          (let* ((event-string (buffer-substring-no-properties (point-at-bol)
                                                               (point-at-eol)))
                 (event-list   (split-string event-string "[ /:]"))
                 (code         (cl-first event-list))
                 (date-time    (--> (seq-drop event-list 1)
                                    (seq-take it 6)
                                    (mapcar #'string-to-number it)
                                    (reverse it)
                                    (apply #'encode-time it)
                                    (chronometrist-format-time-iso8601 it)))
                 (project-or-comment
                  (replace-regexp-in-string
                   (rx (and (or "i" "o") " "
                            (and (= 4 digit) "/" (= 2 digit) "/" (= 2 digit) " ")
                            (and (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
                            (opt " ")))
                   ""
                   event-string)))
            (pcase code
              ("i"
               (cl-incf key-counter)
               (puthash key-counter
                        `(:name ,project-or-comment :start ,date-time)
                        chronometrist-migrate-table))
              ("o"
               (--> (gethash key-counter chronometrist-migrate-table)
                    (append it
                            `(:stop ,date-time)
                            (when (and (stringp project-or-comment)
                                       (not
                                        (string= project-or-comment "")))
                              `(:comment ,project-or-comment)))
                    (puthash key-counter it chronometrist-migrate-table)))))
          (forward-line)
          (goto-char (point-at-bol))))
      nil)))
;; populate:1 ends here

;; [[file:chronometrist.org::*timelog-file-to-sexp-file][timelog-file-to-sexp-file:1]]
(defvar timeclock-file)

(defun chronometrist-migrate-timelog-file-to-sexp-file (&optional in-file out-file)
  "Migrate your existing `timeclock-file' to the Chronometrist file format.
IN-FILE and OUT-FILE, if provided, are used as input and output
file names respectively."
  (interactive `(,(if (featurep 'timeclock)
                      (read-file-name (concat "timeclock file (default: "
                                              timeclock-file
                                              "): ")
                                      user-emacs-directory
                                      timeclock-file t)
                    (read-file-name (concat "timeclock file: ")
                                    user-emacs-directory
                                    nil t))
                 ,(read-file-name (concat "Output file (default: "
                                          (locate-user-emacs-file "chronometrist.sexp")
                                          "): ")
                                  user-emacs-directory
                                  (locate-user-emacs-file "chronometrist.sexp"))))
  (when (if (file-exists-p out-file)
            (yes-or-no-p (concat "Output file "
                                 out-file
                                 " already exists - overwrite? "))
          t)
    (let ((output (find-file-noselect out-file)))
      (with-current-buffer output
        (erase-buffer)
        (chronometrist-migrate-populate in-file)
        (maphash (lambda (_key value)
                   (chronometrist-plist-pp value output)
                   (insert "\n\n"))
                 chronometrist-migrate-table)
        (save-buffer)))))
;; timelog-file-to-sexp-file:1 ends here

;; [[file:chronometrist.org::*check][check:1]]
(defun chronometrist-migrate-check ()
  "Offer to import data from `timeclock-file' if `chronometrist-file' does not exist."
  (when (and (bound-and-true-p timeclock-file)
             (not (file-exists-p chronometrist-file)))
    (if (yes-or-no-p (format (concat "Chronometrist v0.3+ uses a new file format;"
                                     " import data from %s ? ")
                             timeclock-file))
        (chronometrist-migrate-timelog-file-to-sexp-file timeclock-file chronometrist-file)
      (message "You can migrate later using `chronometrist-migrate-timelog-file-to-sexp-file'."))))
;; check:1 ends here

;; [[file:chronometrist.org::*update-interval][update-interval:1]]
(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\")."
  :type 'integer)
;; update-interval:1 ends here

;; [[file:chronometrist.org::*timer-object][timer-object:1]]
(defvar chronometrist--timer-object nil)
;; timer-object:1 ends here

;; [[file:chronometrist.org::*timer-hook][timer-hook:1]]
(defcustom chronometrist-timer-hook nil
  "Functions run by `chronometrist-timer'."
  :type '(repeat function))
;; timer-hook:1 ends here

;; [[file:chronometrist.org::*start-timer][start-timer:1]]
(defun chronometrist-start-timer ()
  (setq chronometrist--timer-object
        (run-at-time t chronometrist-update-interval #'chronometrist-timer)))
;; start-timer:1 ends here

;; [[file:chronometrist.org::*stop-timer][stop-timer:1]]
(defun chronometrist-stop-timer ()
  "Stop the timer for Chronometrist buffers."
  (interactive)
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist--timer-object nil))
;; stop-timer:1 ends here

;; [[file:chronometrist.org::*maybe-start-timer][maybe-start-timer:1]]
(defun chronometrist-maybe-start-timer (&optional interactive-test)
  "Start `chronometrist-timer' if `chronometrist--timer-object' is non-nil.
INTERACTIVE-TEST is used to determine if this has been called
interactively."
  (interactive "p")
  (unless chronometrist--timer-object
    (chronometrist-start-timer)
    (when interactive-test
      (message "Timer started."))
    t))
;; maybe-start-timer:1 ends here

;; [[file:chronometrist.org::*force-restart-timer][force-restart-timer:1]]
(defun chronometrist-force-restart-timer ()
  "Restart the timer for Chronometrist buffers."
  (interactive)
  (when chronometrist--timer-object
    (cancel-timer chronometrist--timer-object))
  (chronometrist-start-timer))
;; force-restart-timer:1 ends here

;; [[file:chronometrist.org::*change-update-interval][change-update-interval:1]]
(defun chronometrist-change-update-interval (arg)
  "Change the update interval for Chronometrist buffers.

ARG should be the new update interval, in seconds."
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))
;; change-update-interval:1 ends here

;; [[file:chronometrist.org::*timer][timer:1]]
(defvar chronometrist-buffer-name)
(defun chronometrist-timer ()
  "Refresh Chronometrist and related buffers.
Buffers will be refreshed only if they are visible, the user is
clocked in to a task, and the active backend is not being
modified."
  ;; No need to update the buffer if there is no active task, or if
  ;; the file is being edited by the user. (The file may be in an
  ;; invalid state, and reading it then may result in a read error.)
  ;; Check for buffer modification first, since `chronometrist-current-task' may
  ;; access the file (causing an error).
  (when (and (not (chronometrist-backend-modified-p (chronometrist-active-backend)))
             (chronometrist-current-task))
    (when (get-buffer-window chronometrist-buffer-name)
      (chronometrist-refresh))
    (run-hooks 'chronometrist-timer-hook)))
;; timer:1 ends here

;; [[file:chronometrist.org::*buffer-name][buffer-name:1]]
(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'."
  :type 'string)
;; buffer-name:1 ends here

;; [[file:chronometrist.org::*hide-cursor][hide-cursor:1]]
(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer."
  :type 'boolean)
;; hide-cursor:1 ends here

;; [[file:chronometrist.org::*activity-indicator][activity-indicator:1]]
(defcustom chronometrist-activity-indicator "*"
  "How to indicate that a task is active.
Can be a string to be displayed, or a function which returns this string.
The default is \"*\""
  :type '(choice string function))
;; activity-indicator:1 ends here

;; [[file:chronometrist.org::*point][point:1]]
(defvar chronometrist--point nil)
;; point:1 ends here

;; [[file:chronometrist.org::*open-log][open-log:1]]
(defun chronometrist-open-log (&optional _button)
  "Open `chronometrist-file' in another window.

Argument _BUTTON is for the purpose of using this command as a
button action."
  (interactive)
  (chronometrist-edit-backend (chronometrist-active-backend)))
;; open-log:1 ends here

;; [[file:chronometrist.org::*task-active-p][task-active-p:1]]
(defun chronometrist-task-active-p (task)
  "Return t if TASK is currently clocked in, else nil."
  (equal (chronometrist-current-task) task))
;; task-active-p:1 ends here

;; [[file:chronometrist.org::*activity-indicator][activity-indicator:1]]
(defun chronometrist-activity-indicator ()
  "Return a string to indicate that a task is active.
See custom variable `chronometrist-activity-indicator'."
  (if (functionp chronometrist-activity-indicator)
      (funcall chronometrist-activity-indicator)
    chronometrist-activity-indicator))
;; activity-indicator:1 ends here

;; [[file:chronometrist.org::*run-transformers][run-transformers:1]]
(defun chronometrist-run-transformers (transformers arg)
  "Run TRANSFORMERS with ARG.
TRANSFORMERS should be a list of functions (F₁ ... Fₙ), each of
which should accept a single argument.

Call F₁ with ARG, with each following function being called with
the return value of the previous function.

Return the value returned by Fₙ."
  (if transformers
      (dolist (fn transformers arg)
        (setq arg (funcall fn arg)))
    arg))
;; run-transformers:1 ends here

;; [[file:chronometrist.org::*schema][schema:1]]
(defcustom chronometrist-schema
  '[("#" 3 t) ("Task" 25 t) ("Time" 10 t) ("Active" 10 t)]
  "Vector specifying schema of `chronometrist' buffer.
See `tabulated-list-format'."
  :type '(vector))
;; schema:1 ends here

;; [[file:chronometrist.org::*chronometrist-mode-hook][chronometrist-mode-hook:1]]
(defvar chronometrist-mode-hook nil
  "Normal hook run at the very end of `chronometrist-mode'.")
;; chronometrist-mode-hook:1 ends here

;; [[file:chronometrist.org::*schema-transformers][schema-transformers:1]]
(defvar chronometrist-schema-transformers nil
  "List of functions to transform `chronometrist-schema'.
This is called with `chronometrist-run-transformers' in `chronometrist-mode', which see.

Extensions using `chronometrist-schema-transformers' to
increase the number of columns will also need to modify the value
of `tabulated-list-entries' by using
`chronometrist-row-transformers'.")
;; schema-transformers:1 ends here

;; [[file:chronometrist.org::*row-transformers][row-transformers:1]]
(defvar chronometrist-row-transformers nil
  "List of functions to transform each row of `tabulated-list-entries'.
This is called with `chronometrist-run-transformers' in `chronometrist-rows', which see.

Extensions using `chronometrist-row-transformers' to increase
the number of columns will also need to modify the value of
`tabulated-list-format' by using
`chronometrist-schema-transformers'.")
;; row-transformers:1 ends here

;; [[file:chronometrist.org::*before-in-functions][before-in-functions:1]]
(defcustom chronometrist-before-in-functions nil
  "Functions to run before a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook."
  :type '(repeat function))
;; before-in-functions:1 ends here

;; [[file:chronometrist.org::*after-in-functions][after-in-functions:1]]
(defcustom chronometrist-after-in-functions nil
  "Functions to run after a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook."
  :type '(repeat function))
;; after-in-functions:1 ends here

;; [[file:chronometrist.org::*before-out-functions][before-out-functions:1]]
(defcustom chronometrist-before-out-functions nil
  "Functions to run before a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.

The task will be stopped only if all functions in this list
return a non-nil value."
  :type '(repeat function))
;; before-out-functions:1 ends here

;; [[file:chronometrist.org::*after-out-functions][after-out-functions:1]]
(defcustom chronometrist-after-out-functions nil
  "Functions to run after a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of."
  :type '(repeat function))
;; after-out-functions:1 ends here

;; [[file:chronometrist.org::*file-change-hook][file-change-hook:1]]
(defcustom chronometrist-file-change-hook nil
  "Functions to be run after `chronometrist-file' is changed on disk."
  :type '(repeat function))
;; file-change-hook:1 ends here

;; [[file:chronometrist.org::*rows][rows:1]]
(defun chronometrist-rows ()
  "Return rows to be displayed in the buffer created by `chronometrist', in the format specified by `tabulated-list-entries'."
  (cl-loop with index = 1
    for task in (-sort #'string-lessp (chronometrist-task-list)) collect
    (let* ((index       (number-to-string index))
           (task-button `(,task action chronometrist-toggle-task-button
                                follow-link t))
           (task-time   (chronometrist-format-duration (chronometrist-task-time-one-day task)))
           (indicator   (if (chronometrist-task-active-p task)
                            (chronometrist-activity-indicator) "")))
      (--> (vector index task-button task-time indicator)
           (list task it)
           (chronometrist-run-transformers chronometrist-row-transformers it)))
    do (cl-incf index)))
;; rows:1 ends here

;; [[file:chronometrist.org::*task-at-point][task-at-point:1]]
(defun chronometrist-task-at-point ()
  "Return the task at point in the `chronometrist' buffer, or nil if there is no task at point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "[0-9]+ +" nil t)
      (get-text-property (point) 'tabulated-list-id))))
;; task-at-point:1 ends here

;; [[file:chronometrist.org::*goto-last-task][goto-last-task:1]]
(defun chronometrist-goto-last-task ()
  "In the `chronometrist' buffer, move point to the line containing the last active task."
  (let* ((latest-record (chronometrist-latest-record (chronometrist-active-backend)))
         (name (plist-get latest-record :name)))
    (goto-char (point-min))
    (re-search-forward name nil t)
    (beginning-of-line)))
;; goto-last-task:1 ends here

;; [[file:chronometrist.org::*print-non-tabular][print-non-tabular:1]]
(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (with-current-buffer chronometrist-buffer-name
    (let ((inhibit-read-only t) (w "\n    "))
      (goto-char (point-max))
      (--> (chronometrist-active-time-on)
           (chronometrist-format-duration it)
           (format "%s%- 26s%s" w "Total" it)
           (insert it)))))
;; print-non-tabular:1 ends here

;; [[file:chronometrist.org::*goto-nth-task][goto-nth-task:1]]
(defun chronometrist-goto-nth-task (n)
  "Move point to the line containing the Nth task.
Return the task at point, or nil if there is no corresponding
task. N must be a positive integer."
  (goto-char (point-min))
  (when (re-search-forward (format "^%d" n) nil t)
    (beginning-of-line)
    (chronometrist-task-at-point)))
;; goto-nth-task:1 ends here

;; [[file:chronometrist.org::*refresh][refresh:1]]
(defun chronometrist-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist' buffer, without re-reading `chronometrist-file'.
The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((window (get-buffer-window chronometrist-buffer-name t))
         (point  (window-point window)))
    (when window
      (with-current-buffer chronometrist-buffer-name
        (tabulated-list-print t nil)
        (chronometrist-print-non-tabular)
        (chronometrist-maybe-start-timer)
        (set-window-point window point)))))
;; refresh:1 ends here

;; [[file:chronometrist.org::*refresh-file][refresh-file:1]]
(defun chronometrist-refresh-file (fs-event)
  "Procedure run when `chronometrist-file' changes.
Re-read `chronometrist-file', update caches, and
refresh the `chronometrist' buffer."
  (run-hooks 'chronometrist-file-change-hook)
  ;; (message "chronometrist - file %s" fs-event)
  (chronometrist-on-change (chronometrist-active-backend) fs-event)
  (chronometrist-refresh))
;; refresh-file:1 ends here

;; [[file:chronometrist.org::*query-stop][query-stop:1]]
(defun chronometrist-query-stop ()
  "Ask the user if they would like to clock out."
  (let ((task (chronometrist-current-task)))
    (and task
         (yes-or-no-p (format "Stop tracking time for %s? " task))
         (chronometrist-out))
    t))
;; query-stop:1 ends here

;; [[file:chronometrist.org::*chronometrist-in][chronometrist-in:1]]
(defun chronometrist-in (task &optional _prefix)
  "Clock in to TASK; record current time in `chronometrist-file'.
TASK is the name of the task, a string. PREFIX is ignored."
  (interactive "P")
  (let ((plist `(:name ,task :start ,(chronometrist-format-time-iso8601))))
    (chronometrist-insert (chronometrist-active-backend) plist)
    (chronometrist-refresh)))
;; chronometrist-in:1 ends here

;; [[file:chronometrist.org::*chronometrist-out][chronometrist-out:1]]
(defun chronometrist-out (&optional _prefix)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.
PREFIX is ignored."
  (interactive "P")
  (let* ((latest (chronometrist-latest-record (chronometrist-active-backend)))
         (plist  (plist-put latest :stop (chronometrist-format-time-iso8601))))
    (chronometrist-replace-last (chronometrist-active-backend) plist)))
;; chronometrist-out:1 ends here

;; [[file:chronometrist.org::*run-functions-and-clock-in][run-functions-and-clock-in:1]]
(defun chronometrist-run-functions-and-clock-in (task)
  "Run hooks and clock in to TASK."
  (run-hook-with-args 'chronometrist-before-in-functions task)
  (chronometrist-in task)
  (run-hook-with-args 'chronometrist-after-in-functions task))
;; run-functions-and-clock-in:1 ends here

;; [[file:chronometrist.org::*run-functions-and-clock-out][run-functions-and-clock-out:1]]
(defun chronometrist-run-functions-and-clock-out (task)
  "Run hooks and clock out of TASK."
  (when (run-hook-with-args-until-failure 'chronometrist-before-out-functions task)
    (chronometrist-out)
    (run-hook-with-args 'chronometrist-after-out-functions task)))
;; run-functions-and-clock-out:1 ends here

;; [[file:chronometrist.org::*chronometrist-mode-map][chronometrist-mode-map:1]]
(defvar chronometrist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")          #'chronometrist-add-new-task)
    (define-key map (kbd "RET")        #'chronometrist-toggle-task)
    (define-key map (kbd "M-RET")      #'chronometrist-toggle-task-no-hooks)
    (define-key map [mouse-1]          #'chronometrist-toggle-task)
    (define-key map [mouse-3]          #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "<C-return>") #'chronometrist-restart-task)
    (define-key map (kbd "<C-M-return>") #'chronometrist-extend-task)
    (define-key map (kbd "D")          #'chronometrist-discard-active)
    (define-key map (kbd "d")          #'chronometrist-details)
    (define-key map (kbd "r")          #'chronometrist-report)
    (define-key map (kbd "l")          #'chronometrist-open-log)
    (define-key map (kbd "G")          #'chronometrist-reset)
    (define-key map (kbd "T")          #'chronometrist-force-restart-timer)
    map)
  "Keymap used by `chronometrist-mode'.")
;; chronometrist-mode-map:1 ends here

;; [[file:chronometrist.org::*chronometrist-menu][chronometrist-menu:1]]
(easy-menu-define chronometrist-menu chronometrist-mode-map
  "Chronometrist mode menu."
  '("Chronometrist"
    ["Start a new task" chronometrist-add-new-task]
    ["Toggle task at point" chronometrist-toggle-task]
    ["Toggle task without running hooks" chronometrist-toggle-task-no-hooks]
    ["Discard and restart active task" chronometrist-restart-task]
    ["Discard and restart without running hooks" (chronometrist-restart-task t)
     :keys "\\[universal-argument] \\[chronometrist-restart-task]"]
    ["Extend time for last completed task" chronometrist-extend-task]
    ["Extend time without running hooks" (chronometrist-extend-task t)
     :keys "\\[universal-argument] \\[chronometrist-extend-task]"]
    ["Discard active interval" chronometrist-discard-active]
    "----"
    ["View details of today's data" chronometrist-details]
    ["View weekly report" chronometrist-report]
    ["View/edit log file" chronometrist-open-log]
    ["View/edit literate source" chronometrist-open-literate-source]
    "----"
    ["Restart timer" chronometrist-force-restart-timer]
    ["Reset state" chronometrist-reset]
    ["Import/export data" chronometrist-migrate]))
;; chronometrist-menu:1 ends here

;; [[file:chronometrist.org::*chronometrist-mode][chronometrist-mode:1]]
(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (make-local-variable 'tabulated-list-format)
  (--> (chronometrist-run-transformers chronometrist-schema-transformers chronometrist-schema)
    (setq tabulated-list-format it))
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-rows)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function #'chronometrist-refresh)
  (run-hooks 'chronometrist-mode-hook))
;; chronometrist-mode:1 ends here

;; [[file:chronometrist.org::*toggle-task-button][toggle-task-button:1]]
(defun chronometrist-toggle-task-button (_button)
  "Button action to toggle a task.
Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (when current-prefix-arg
    (chronometrist-goto-nth-task (prefix-numeric-value current-prefix-arg)))
  (let ((current  (chronometrist-current-task))
        (at-point (chronometrist-task-at-point)))
    ;; clocked in + point on current    = clock out
    ;; clocked in + point on some other task = clock out, clock in to task
    ;; clocked out = clock in
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (unless (equal at-point current)
      (chronometrist-run-functions-and-clock-in at-point))))
;; toggle-task-button:1 ends here

;; [[file:chronometrist.org::*add-new-task-button][add-new-task-button:1]]
(defun chronometrist-add-new-task-button (_button)
  "Button action to add a new task.
Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (let ((current (chronometrist-current-task)))
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (let ((task (read-from-minibuffer "New task name: " nil nil nil nil nil t)))
      (chronometrist-run-functions-and-clock-in task))))
;; add-new-task-button:1 ends here

;; [[file:chronometrist.org::*toggle-task][toggle-task:1]]
;; TODO - if clocked in and point not on a task, just clock out
(defun chronometrist-toggle-task (&optional prefix inhibit-hooks)
  "Start or stop the task at point.

If there is no task at point, do nothing.

With numeric prefix argument PREFIX, toggle the Nth task in
the buffer. If there is no corresponding task, do nothing.

If INHIBIT-HOOKS is non-nil, the hooks
`chronometrist-before-in-functions',
`chronometrist-after-in-functions',
`chronometrist-before-out-functions', and
`chronometrist-after-out-functions' will not be run."
  (interactive "P")
  (chronometrist-debug-message "[Command] toggle-task %s" (if inhibit-hooks "(without hooks)" ""))
  (let* ((empty-file   (chronometrist-backend-empty-p (chronometrist-active-backend)))
         (nth          (when prefix (chronometrist-goto-nth-task prefix)))
         (at-point     (chronometrist-task-at-point))
         (target       (or nth at-point))
         (current      (chronometrist-current-task))
         (in-function  (if inhibit-hooks #'chronometrist-in  #'chronometrist-run-functions-and-clock-in))
         (out-function (if inhibit-hooks #'chronometrist-out #'chronometrist-run-functions-and-clock-out)))
    ;; do not run hooks - chronometrist-add-new-task will do it
    (cond (empty-file (chronometrist-add-new-task))
          ;; What should we do if the user provides an invalid
          ;; argument? Currently - nothing.
          ((and prefix (not nth)))
          (target ;; do nothing if there's no task at point
           ;; clocked in + target is current = clock out
           ;; clocked in + target is some other task = clock out, clock in to task
           ;; clocked out = clock in
           (when current
             (funcall out-function current))
           (unless (equal target current)
             (funcall in-function target))))))
;; toggle-task:1 ends here

;; [[file:chronometrist.org::*toggle-task-no-hooks][toggle-task-no-hooks:1]]
(defun chronometrist-toggle-task-no-hooks (&optional prefix)
  "Like `chronometrist-toggle-task', but don't run hooks.

With numeric prefix argument PREFIX, toggle the Nth task. If
there is no corresponding task, do nothing."
  (interactive "P")
  (chronometrist-toggle-task prefix t))
;; toggle-task-no-hooks:1 ends here

;; [[file:chronometrist.org::*add-new-task][add-new-task:1]]
(defun chronometrist-add-new-task ()
  "Add a new task."
  (interactive)
  (chronometrist-debug-message "[Command] add-new-task")
  (chronometrist-add-new-task-button nil))
;; add-new-task:1 ends here

;; [[file:chronometrist.org::*restart-task][restart-task:1]]
(defun chronometrist-restart-task (&optional inhibit-hooks)
  "Change the start time of the active task to the current time.
`chronometrist-before-in-functions' and
`chronometrist-after-in-functions' are run again, unless
INHIBIT-HOOKS is non-nil or prefix argument is supplied.

Has no effect if no task is active."
  (interactive "P")
  (chronometrist-debug-message "[Command] restart-task")
  (if (chronometrist-current-task)
      (let* ((latest (chronometrist-latest-record (chronometrist-active-backend)))
             (plist  (plist-put latest :start (chronometrist-format-time-iso8601)))
             (task   (plist-get plist :name)))
        (unless inhibit-hooks
         (run-hook-with-args 'chronometrist-before-in-functions task))
        (chronometrist-replace-last (chronometrist-active-backend) plist)
        (unless inhibit-hooks
         (run-hook-with-args 'chronometrist-after-in-functions task)))
    (message "Can only restart an active task - use this when clocked in.")))
;; restart-task:1 ends here

;; [[file:chronometrist.org::*extend-task][extend-task:1]]
(defun chronometrist-extend-task (&optional inhibit-hooks)
  "Change the stop time of the last task to the current time.
`chronometrist-before-out-functions' and
`chronometrist-after-out-functions' are run again, unless
INHIBIT-HOOKS is non-nil or prefix argument is supplied.

Has no effect if a task is active."
  (interactive "P")
  (chronometrist-debug-message "[Command] extend-task")
  (if (chronometrist-current-task)
      (message "Cannot extend an active task - use this after clocking out.")
    (let* ((latest (chronometrist-latest-record (chronometrist-active-backend)))
           (plist  (plist-put latest :stop (chronometrist-format-time-iso8601)))
           (task   (plist-get plist :name)))
      (unless inhibit-hooks
         (run-hook-with-args-until-failure 'chronometrist-before-out-functions task))
      (chronometrist-replace-last (chronometrist-active-backend) plist)
      (unless inhibit-hooks
        (run-hook-with-args 'chronometrist-after-out-functions task)))))
;; extend-task:1 ends here

;; [[file:chronometrist.org::*discard-active][discard-active:1]]
(defun chronometrist-discard-active ()
  "Remove active interval from the active backend."
  (interactive)
  (chronometrist-debug-message "[Command] discard-active")
  (let ((backend (chronometrist-active-backend)))
    (if (chronometrist-current-task backend)
        (chronometrist-remove-last backend)
      (message "Nothing to discard - use this when clocked in."))))
;; discard-active:1 ends here

;; [[file:chronometrist.org::*chronometrist][chronometrist:1]]
;;;###autoload
(defun chronometrist (&optional arg)
  "Display the user's tasks and the time spent on them today.
If numeric argument ARG is 1, run `chronometrist-report'; if 2,
run `chronometrist-statistics'."
  (interactive "P")
  (chronometrist-migrate-check)
  (let* ((buffer (get-buffer-create chronometrist-buffer-name))
         (w      (save-excursion
                   (get-buffer-window chronometrist-buffer-name t)))
         (backend (chronometrist-active-backend)))
    (cond
     (arg (cl-case arg
            (1 (chronometrist-report))
            (2 (chronometrist-statistics))))
     (w (with-current-buffer buffer
          (setq chronometrist--point (point))
          (kill-buffer chronometrist-buffer-name)))
     (t (with-current-buffer buffer
          (cond ((chronometrist-backend-empty-p backend)
                 ;; database is empty
                 (chronometrist-create-file backend)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert "Welcome to Chronometrist! Hit RET to ")
                   (insert-text-button "start a new task."
                                       'action #'chronometrist-add-new-task-button
                                       'follow-link t)
                   (chronometrist-mode)
                   (switch-to-buffer buffer)))
                (t (chronometrist-mode)
                   (when chronometrist-hide-cursor
                     (make-local-variable 'cursor-type)
                     (setq cursor-type nil)
                     (hl-line-mode))
                   (switch-to-buffer buffer)
                   (when (chronometrist-memory-layer-empty-p backend)
                     (chronometrist-reset-backend backend))
                   (chronometrist-refresh)
                   (if chronometrist--point
                       (goto-char chronometrist--point)
                     (chronometrist-goto-last-task))))
          (chronometrist-setup-file-watch))))))
;; chronometrist:1 ends here

;; [[file:chronometrist.org::*report][report:1]]
(defgroup chronometrist-report nil
  "Weekly report for the `chronometrist' time tracker."
  :group 'chronometrist)
;; report:1 ends here

;; [[file:chronometrist.org::*buffer-name][buffer-name:1]]
(defcustom chronometrist-report-buffer-name "*Chronometrist-Report*"
  "The name of the buffer created by `chronometrist-report'."
  :type 'string)
;; buffer-name:1 ends here

;; [[file:chronometrist.org::*ui-date][ui-date:1]]
(defvar chronometrist-report--ui-date nil
  "The first date of the week displayed by `chronometrist-report'.
A value of nil means the current week. Otherwise, it must be a
date in the form \"YYYY-MM-DD\".")
;; ui-date:1 ends here

;; [[file:chronometrist.org::*ui-week-dates][ui-week-dates:1]]
(defvar chronometrist-report--ui-week-dates nil
  "List of dates currently displayed by `chronometrist-report'.
Each date is a list containing calendrical information (see (info \"(elisp)Time Conversion\"))")
;; ui-week-dates:1 ends here

;; [[file:chronometrist.org::*point][point:1]]
(defvar chronometrist-report--point nil)
;; point:1 ends here

;; [[file:chronometrist.org::*date-to-dates-in-week][date-to-dates-in-week:1]]
(defun chronometrist-report-date-to-dates-in-week (first-date-in-week)
  "Return a list of dates in a week, starting from FIRST-DATE-IN-WEEK.
Each date is a ts struct (see `ts.el').

FIRST-DATE-IN-WEEK must be a ts struct representing the first date."
  (cl-loop for i from 0 to 6 collect
           (ts-adjust 'day i first-date-in-week)))
;; date-to-dates-in-week:1 ends here

;; [[file:chronometrist.org::*date-to-week-dates][date-to-week-dates:1]]
(defun chronometrist-report-date-to-week-dates ()
  "Return dates in week as a list.
Each element is a ts struct (see `ts.el').

The first date is the first occurrence of
`chronometrist-report-week-start-day' before the date specified in
`chronometrist-report--ui-date' (if non-nil) or the current date."
  (->> (or chronometrist-report--ui-date (chronometrist-date-ts))
       (chronometrist-previous-week-start)
       (chronometrist-report-date-to-dates-in-week)))
;; date-to-week-dates:1 ends here

;; [[file:chronometrist.org::*rows][rows:1]]
(defun chronometrist-report-rows ()
  "Return rows to be displayed in the `chronometrist-report' buffer."
  (cl-loop
    ;; `chronometrist-report-date-to-week-dates' uses today if chronometrist-report--ui-date is nil
    with week-dates = (setq chronometrist-report--ui-week-dates
                            (chronometrist-report-date-to-week-dates))
    for task in (chronometrist-task-list) collect
    (let* ((durations        (--map (chronometrist-task-time-one-day task (chronometrist-date-ts it))
                                    week-dates))
           (duration-strings (mapcar #'chronometrist-format-duration durations))
           (total-duration   (->> (-reduce #'+ durations)
                                  (chronometrist-format-duration)
                                  (vector))))
      (list task
            (vconcat
             (vector task)
             duration-strings ;; vconcat converts lists to vectors
             total-duration)))))
;; rows:1 ends here

;; [[file:chronometrist.org::*print-keybind][print-keybind:1]]
(defun chronometrist-report-print-keybind (command &optional description firstonly)
  "Insert one or more keybindings for COMMAND into the current buffer.
DESCRIPTION is a description of the command.

If FIRSTONLY is non-nil, insert only the first keybinding found."
  (insert "\n    "
          (chronometrist-format-keybinds command firstonly)
          " - "
          (if description description "")))
;; print-keybind:1 ends here

;; [[file:chronometrist.org::*print-non-tabular][print-non-tabular:1]]
(defun chronometrist-report-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-report'."
  (let* ((inhibit-read-only t)
         (w "\n    ")
         (ui-week-dates-ts  (mapcar #'chronometrist-date-ts chronometrist-report--ui-week-dates))
         (total-time-daily  (mapcar #'chronometrist-active-time-on
                                    ui-week-dates-ts)))
    (goto-char (point-min))
    (insert (make-string 25 ?\s))
    (insert (mapconcat (lambda (ts)
                         (ts-format "%F" ts))
                       (chronometrist-report-date-to-week-dates)
                       " "))
    (insert "\n")
    (goto-char (point-max))
    (insert w (format "%- 21s" "Total"))
    (->> (mapcar #'chronometrist-format-duration total-time-daily)
         (--map (format "% 9s  " it))
         (apply #'insert))
    (->> (-reduce #'+ total-time-daily)
         (chronometrist-format-duration)
         (format "% 13s")
         (insert))
    (insert "\n" w)
    (insert-text-button "<<" 'action #'chronometrist-report-previous-week 'follow-link t)
    (insert (format "% 4s" " "))
    (insert-text-button ">>" 'action #'chronometrist-report-next-week 'follow-link t)
    (insert "\n")
    (chronometrist-report-print-keybind 'chronometrist-report-previous-week)
    (insert-text-button "previous week" 'action #'chronometrist-report-previous-week 'follow-link t)
    (chronometrist-report-print-keybind 'chronometrist-report-next-week)
    (insert-text-button "next week" 'action #'chronometrist-report-next-week 'follow-link t)
    (chronometrist-report-print-keybind 'chronometrist-open-log)
    (insert-text-button "open log file" 'action #'chronometrist-open-log 'follow-link t)))
;; print-non-tabular:1 ends here

;; [[file:chronometrist.org::*refresh][refresh:1]]
(defun chronometrist-report-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist-report' buffer, without re-reading `chronometrist-file'."
  (let* ((w (get-buffer-window chronometrist-report-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-report-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-report-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))
;; refresh:1 ends here

;; [[file:chronometrist.org::*refresh-file][refresh-file:1]]
(defun chronometrist-report-refresh-file (fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist-report' buffer."
  (run-hooks 'chronometrist-file-change-hook)
  (chronometrist-on-change (chronometrist-active-backend) fs-event)
  (chronometrist-report-refresh))
;; refresh-file:1 ends here

;; [[file:chronometrist.org::*report-mode-map][report-mode-map:1]]
(defvar chronometrist-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-log)
    (define-key map (kbd "b") #'chronometrist-report-previous-week)
    (define-key map (kbd "f") #'chronometrist-report-next-week)
    ;; Works when number of tasks < screen length; after that, you
    ;; probably expect mousewheel to scroll up/down, and
    ;; alt-mousewheel or something for next/previous week. For now,
    ;; I'm assuming most people won't have all that many tasks - I've
    ;; been using it for ~2 months and have 18 tasks, which are
    ;; still just half the screen on my 15" laptop. Let's see what
    ;; people say.
    (define-key map [mouse-4] #'chronometrist-report-next-week)
    (define-key map [mouse-5] #'chronometrist-report-previous-week)
    map)
  "Keymap used by `chronometrist-report-mode'.")
;; report-mode-map:1 ends here

;; [[file:chronometrist.org::*report-mode][report-mode:1]]
(define-derived-mode chronometrist-report-mode tabulated-list-mode "Chronometrist-Report"
  "Major mode for `chronometrist-report'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Task"   25 t)
                               ("Sunday"    10 t)
                               ("Monday"    10 t)
                               ("Tuesday"   10 t)
                               ("Wednesday" 10 t)
                               ("Thursday"  10 t)
                               ("Friday"    10 t)
                               ("Saturday"  10 t :pad-right 5)
                               ("Total"     12 t)])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-report-rows)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (chronometrist-maybe-start-timer)
  (add-hook 'chronometrist-timer-hook
            (lambda ()
              (when (get-buffer-window chronometrist-report-buffer-name)
                (chronometrist-report-refresh))))
  (setq revert-buffer-function #'chronometrist-report-refresh)
  (chronometrist-setup-file-watch))
;; report-mode:1 ends here

;; [[file:chronometrist.org::*chronometrist-report][chronometrist-report:1]]
;;;###autoload
(defun chronometrist-report (&optional keep-date)
  "Display a weekly report of the data in `chronometrist-file'.
If a buffer called `chronometrist-report-buffer-name' already
exists and is visible, kill the buffer.

If KEEP-DATE is nil (the default when not supplied), set
`chronometrist-report--ui-date' to nil and display data from the
current week. Otherwise, display data from the week specified by
`chronometrist-report--ui-date'."
  (interactive)
  (chronometrist-migrate-check)
  (let ((buffer (get-buffer-create chronometrist-report-buffer-name)))
    (with-current-buffer buffer
      (cond ((and (get-buffer-window chronometrist-report-buffer-name)
                  (not keep-date))
             (setq chronometrist-report--point (point))
             (kill-buffer buffer))
            (t (unless keep-date
                 (setq chronometrist-report--ui-date nil))
               (chronometrist-create-file (chronometrist-active-backend))
               (chronometrist-report-mode)
               (switch-to-buffer buffer)
               (chronometrist-report-refresh-file nil)
               (goto-char (or chronometrist-report--point 1)))))))
;; chronometrist-report:1 ends here

;; [[file:chronometrist.org::*report-previous-week][report-previous-week:1]]
(defun chronometrist-report-previous-week (arg)
  "View the previous week's report.
With prefix argument ARG, move back ARG weeks."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (setq chronometrist-report--ui-date
          (ts-adjust 'day (- (* arg 7))
                     (if chronometrist-report--ui-date
                         chronometrist-report--ui-date
                       (ts-now)))))
  (setq chronometrist-report--point (point))
  (kill-buffer)
  (chronometrist-report t))
;; report-previous-week:1 ends here

;; [[file:chronometrist.org::*report-next-week][report-next-week:1]]
(defun chronometrist-report-next-week (arg)
  "View the next week's report.
With prefix argument ARG, move forward ARG weeks."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (setq chronometrist-report--ui-date
          (ts-adjust 'day (* arg 7)
                     (if chronometrist-report--ui-date
                         chronometrist-report--ui-date
                       (ts-now))))
    (setq chronometrist-report--point (point))
    (kill-buffer)
    (chronometrist-report t)))
;; report-next-week:1 ends here

;; [[file:chronometrist.org::*statistics][statistics:1]]
(defgroup chronometrist-statistics nil
  "Statistics buffer for the `chronometrist' time tracker."
  :group 'chronometrist)
;; statistics:1 ends here

;; [[file:chronometrist.org::*buffer-name][buffer-name:1]]
(defcustom chronometrist-statistics-buffer-name "*Chronometrist-Statistics*"
  "The name of the buffer created by `chronometrist-statistics'."
  :type 'string)
;; buffer-name:1 ends here

;; [[file:chronometrist.org::*ui-state][ui-state:1]]
(defvar chronometrist-statistics--ui-state nil
  "Stores the display state for `chronometrist-statistics'.

This must be a plist in the form (:MODE :START :END).

:MODE is either 'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics for all available data at once.

'custom means display statistics from an arbitrary date range.

:START and :END are the start and end of the date range to be
displayed. They must be ts structs (see `ts.el').")
;; ui-state:1 ends here

;; [[file:chronometrist.org::*point][point:1]]
(defvar chronometrist-statistics--point nil)
;; point:1 ends here

;; [[file:chronometrist.org::*mode-map][mode-map:1]]
(defvar chronometrist-statistics-mode-map)
;; mode-map:1 ends here

;; [[file:chronometrist.org::*count-average-time-spent][count-average-time-spent:1]]
(cl-defun chronometrist-statistics-count-average-time-spent (task &optional (backend (chronometrist-active-backend)))
  "Return the average time the user has spent on TASK in BACKEND."
  (cl-loop with days = 0
    with events-in-day
    for date being the hash-keys of (chronometrist-backend-hash-table backend)
    when (setq events-in-day (chronometrist-task-records-for-date backend task date))
    do (cl-incf days) and
    collect
    (-reduce #'+ (chronometrist-events-to-durations events-in-day))
    into per-day-time-list
    finally return
    (if per-day-time-list
        (/ (-reduce #'+ per-day-time-list) days)
      0)))
;; count-average-time-spent:1 ends here

;; [[file:chronometrist.org::*rows-internal][rows-internal:1]]
(defun chronometrist-statistics-rows-internal (table)
  "Helper function for `chronometrist-statistics-rows'.

It simply operates on the entire hash table TABLE (see
`chronometrist-to-hash-table' for table format), so ensure that TABLE is
reduced to the desired range using
`chronometrist-events-subset'."
  (cl-loop for task in (chronometrist-task-list) collect
    (let* ((active-days    (chronometrist-statistics-count-active-days task table))
           (active-percent (cl-case (plist-get chronometrist-statistics--ui-state :mode)
                             ('week (* 100 (/ active-days 7.0)))))
           (active-percent (if (zerop active-days)
                               (format "    % 6s" "-")
                             (format "    %05.2f%%" active-percent)))
           (active-days    (format "% 5s"
                                   (if (zerop active-days)
                                       "-"
                                     active-days)))
           (average-time   (->> (chronometrist-statistics-count-average-time-spent task table)
                             (chronometrist-format-duration)
                             (format "% 5s")))
           (content        (vector task active-days active-percent average-time)))
      (list task content))))
;; rows-internal:1 ends here

;; [[file:chronometrist.org::*rows][rows:1]]
(defun chronometrist-statistics-rows ()
  "Return rows to be displayed in the buffer created by `chronometrist-statistics'."
  ;; We assume that all fields in `chronometrist-statistics--ui-state' are set, so they must
  ;; be changed by the view-changing functions.
  (with-slots (hash-table) (chronometrist-active-backend)
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((start (plist-get chronometrist-statistics--ui-state :start))
              (end   (plist-get chronometrist-statistics--ui-state :end))
              (ht    (chronometrist-events-subset start end hash-table)))
         (chronometrist-statistics-rows-internal ht)))
      (t ;; `chronometrist-statistics--ui-state' is nil, show current week's data
       (let* ((start (chronometrist-previous-week-start (chronometrist-date-ts)))
              (end   (ts-adjust 'day 7 start))
              (ht    (chronometrist-events-subset start end hash-table)))
         (setq chronometrist-statistics--ui-state `(:mode week :start ,start :end ,end))
         (chronometrist-statistics-rows-internal ht))))))
;; rows:1 ends here

;; [[file:chronometrist.org::*print-keybind][print-keybind:1]]
(defun chronometrist-statistics-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert "\n    "
          (chronometrist-format-keybinds command
                             chronometrist-statistics-mode-map
                             firstonly)
          " - "
          (if description description "")))
;; print-keybind:1 ends here

;; [[file:chronometrist.org::*print-non-tabular][print-non-tabular:1]]
(defun chronometrist-statistics-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-statistics'."
  (let ((w "\n    ")
        (inhibit-read-only t))
    (goto-char (point-max))
    (insert w)
    (insert-text-button (cl-case (plist-get chronometrist-statistics--ui-state :mode)
                          ('week "Weekly view"))
                        ;; 'action #'chronometrist-report-previous-week ;; TODO - make interactive function to accept new mode from user
                        'follow-link t)
    (insert ", from")
    (insert
     (format " %s to %s\n"
             (ts-format "%F" (plist-get chronometrist-statistics--ui-state :start))
             (ts-format "%F" (plist-get chronometrist-statistics--ui-state :end))))))
;; print-non-tabular:1 ends here

;; [[file:chronometrist.org::*refresh][refresh:1]]
(defun chronometrist-statistics-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist-statistics' buffer.
This does not re-read `chronometrist-file'.

The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((w (get-buffer-window chronometrist-statistics-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-statistics-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-statistics-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))
;; refresh:1 ends here

;; [[file:chronometrist.org::*mode-map][mode-map:1]]
(defvar chronometrist-statistics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-log)
    (define-key map (kbd "b") #'chronometrist-statistics-previous-range)
    (define-key map (kbd "f") #'chronometrist-statistics-next-range)
    map)
  "Keymap used by `chronometrist-statistics-mode'.")
;; mode-map:1 ends here

;; [[file:chronometrist.org::*statistics-mode][statistics-mode:1]]
(define-derived-mode chronometrist-statistics-mode tabulated-list-mode "Chronometrist-Statistics"
  "Major mode for `chronometrist-statistics'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        [("Task"              25 t)
         ("Active days"       12 t)
         ("%% of days active" 17 t)
         ("Average time"      12 t)
         ;; ("Current streak"    10 t)
         ;; ("Last streak"       10 t)
         ;; ("Longest streak"    10 t)
         ])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-statistics-rows)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  ;; (chronometrist-maybe-start-timer)
  (add-hook 'chronometrist-timer-hook
            (lambda ()
              (when (get-buffer-window chronometrist-statistics-buffer-name)
                (chronometrist-statistics-refresh))))
  (setq revert-buffer-function #'chronometrist-statistics-refresh)
  (chronometrist-setup-file-watch))
;; statistics-mode:1 ends here

;; [[file:chronometrist.org::*chronometrist-statistics][chronometrist-statistics:1]]
;;;###autoload
(defun chronometrist-statistics (&optional preserve-state)
  "Display statistics for Chronometrist data.
If a buffer called `chronometrist-statistics-buffer-name' already
exists and is visible, kill the buffer.

If PRESERVE-STATE is nil (the default when not supplied), display
data from the current week. Otherwise, display data from the week
specified by `chronometrist-statistics--ui-state'."
  (interactive)
  (chronometrist-migrate-check)
  (let* ((buffer     (get-buffer-create chronometrist-statistics-buffer-name))
         (today      (chronometrist-date-ts))
         (week-start (chronometrist-previous-week-start today))
         (week-end   (ts-adjust 'day 6 week-start)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
             (unless preserve-state
               (setq chronometrist-statistics--ui-state `(:mode week
                                         :start ,week-start
                                         :end   ,week-end)))
             (chronometrist-create-file (chronometrist-active-backend))
             (chronometrist-statistics-mode)
             (switch-to-buffer buffer)
             (chronometrist-statistics-refresh))))))
;; chronometrist-statistics:1 ends here

;; [[file:chronometrist.org::*previous-range][previous-range:1]]
(defun chronometrist-statistics-previous-range (arg)
  "View the statistics in the previous time range.
If ARG is a numeric argument, go back that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start)))
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (ts-adjust 'day (- (* arg 7)) start))
              (new-end   (ts-adjust 'day +6 new-start)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end   new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))
;; previous-range:1 ends here

;; [[file:chronometrist.org::*next-range][next-range:1]]
(defun chronometrist-statistics-next-range (arg)
  "View the statistics in the next time range.
If ARG is a numeric argument, go forward that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start)))
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (ts-adjust 'day (* arg 7) start))
              (new-end   (ts-adjust 'day 6 new-start)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end   new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))
;; next-range:1 ends here

;; [[file:chronometrist.org::*details][details:1]]
(defgroup chronometrist-details nil
  "Details buffer for the `chronometrist' time tracker."
  :group 'chronometrist)
;; details:1 ends here

;; [[file:chronometrist.org::*buffer-name-base][buffer-name-base:1]]
(defcustom chronometrist-details-buffer-name-base "chronometrist-details"
  "Name of buffer created by `chronometrist-details'."
  :type 'string)
;; buffer-name-base:1 ends here

;; [[file:chronometrist.org::*buffer-name][buffer-name:1]]
(defun chronometrist-details-buffer-name (&optional suffix)
  "Return buffer name based on `chronometrist-details-buffer-name-base' and SUFFIX."
  (if suffix
      (format "*%s_%s*" chronometrist-details-buffer-name-base suffix)
    (format "*%s*" chronometrist-details-buffer-name-base)))
;; buffer-name:1 ends here

;; [[file:chronometrist.org::*display-tags][display-tags:1]]
(defcustom chronometrist-details-display-tags "%s"
  "How to display tags in `chronometrist-details' buffers.
Value can be
nil, meaning do not display tags, or
a format string consuming a single argument passed to `format', or
a function of one argument (the tags, as a list of symbols),
which must return the string to be displayed.

To disable display of tags, customize `chronometrist-details-schema'."
  :type '(choice nil string function))
;; display-tags:1 ends here

;; [[file:chronometrist.org::*display-key-values][display-key-values:1]]
(defcustom chronometrist-details-display-key-values "%s"
  "How to display tags in `chronometrist-details' buffers.
Value can be
nil, meaning do not display key-values, or
a format string consuming a single argument passed to `format', or
a function of one argument (the full interval plist),
which must return the string to be displayed.

To disable display of key-values, set this to nil and customize
`chronometrist-details-schema'."
  :type '(choice nil string function))
;; display-key-values:1 ends here

;; [[file:chronometrist.org::*time-format-string][time-format-string:1]]
(defcustom chronometrist-details-time-format-string "%H:%M"
  "String specifying time format in `chronometrist-details' buffers.
See `format-time-string'."
  :type 'string)
;; time-format-string:1 ends here

;; [[file:chronometrist.org::*schema][schema:1]]
(defcustom chronometrist-details-schema
  [("#" 3 (lambda (row-1 row-2)
            (< (car row-1)
               (car row-2))))
   ("Task" 20 t)
   ("Tags" 20 t)
   ("Details" 45 t)
   ("Duration" 20 t :right-align t :pad-right 3)
   ("Time" 10 t)]
  "Vector specifying format of `chronometrist-details' buffer.
See `tabulated-list-format'."
  :type '(vector))
;; schema:1 ends here

;; [[file:chronometrist.org::*schema-transformers][schema-transformers:1]]
(defvar chronometrist-details-schema-transformers nil
  "List of functions to transform `chronometrist-details-schema' (which see).
This is passed to `chronometrist-run-transformers', which see.

Extensions adding to this list to increase the number of columns
will also need to modify the value of `tabulated-list-entries' by
using `chronometrist-details-row-transformers'.")
;; schema-transformers:1 ends here

;; [[file:chronometrist.org::*rows-helper][rows-helper:1]]
(defun chronometrist-details-rows-helper (list)
  "Return LIST as a string to be inserted in a `chronometrist-details' buffer.
LIST is either tags (a list of symbols) or a plist."
  (let (contents custom)
    (if (chronometrist-plist-p list)
        (setq custom   chronometrist-details-display-key-values
              contents (seq-remove #'keywordp
                                   (chronometrist-plist-key-values list)))
      (setq custom   chronometrist-details-display-tags
            contents list))
    (if (and contents custom)
        (pcase custom
          ((pred stringp)
           (--> (flatten-list contents)
             (seq-remove #'keywordp it)
             (mapconcat
              (lambda (elt) (format custom elt))
              it ", ")))
          ((pred functionp)
           (funcall custom list)))
      "")))
;; rows-helper:1 ends here

;; [[file:chronometrist.org::*row-transformers][row-transformers:1]]
(defvar chronometrist-details-row-transformers nil
  "List of functions to transform each row of `chronometrist-details-rows'.
This is passed to `chronometrist-run-transformers', which see.

Extensions adding to this list to increase the number of columns
will also need to modify the value of `tabulated-list-format' by
using `chronometrist-details-schema-transformers'.")
;; row-transformers:1 ends here

;; [[file:chronometrist.org::*map][map:1]]
(defvar chronometrist-details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s r") #'chronometrist-details-set-range)
    (define-key map (kbd "s f") #'chronometrist-details-set-filter)
    (define-key map (kbd "r") #'chronometrist-report)
    (define-key map (kbd "l") #'chronometrist-open-log)
    (define-key map (kbd "G") #'chronometrist-reset)
    map))
;; map:1 ends here

;; [[file:chronometrist.org::*chronometrist-details-menu][chronometrist-details-menu:1]]
(easy-menu-define chronometrist-details-menu chronometrist-details-mode-map
  "Menu for `chronometrist-details'."
  '("Details"
    ["Set date/time range" chronometrist-details-set-range]
    ["Set interval filter" chronometrist-details-set-filter]
    ["View weekly report" chronometrist-report]
    ["View/edit log file" chronometrist-open-log]
    ["Reset state" chronometrist-reset]))
;; chronometrist-details-menu:1 ends here

;; [[file:chronometrist.org::*chronometrist-details-mode][chronometrist-details-mode:1]]
(define-derived-mode chronometrist-details-mode tabulated-list-mode "Details"
  "Major mode for `chronometrist-details'."
  (make-local-variable 'tabulated-list-format)
  (--> (chronometrist-run-transformers chronometrist-details-schema-transformers chronometrist-details-schema)
    (setq tabulated-list-format it))
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries #'chronometrist-details-rows)
  (make-local-variable 'tabulated-list-sort-key)
  (tabulated-list-init-header)
  (run-hooks 'chronometrist-mode-hook))
;; chronometrist-details-mode:1 ends here

;; [[file:chronometrist.org::*details-setup-buffer][details-setup-buffer:1]]
(defun chronometrist-details-setup-buffer (buffer-or-name)
  "Enable `chronometrist-details-mode' in BUFFER-OR-NAME and switch to it.
BUFFER-OR-NAME must be an existing buffer."
  (with-current-buffer buffer-or-name
    (switch-to-buffer buffer-or-name)
    (chronometrist-details-mode)
    (tabulated-list-print)))
;; details-setup-buffer:1 ends here

;; [[file:chronometrist.org::*chronometrist-details][chronometrist-details:1]]
(defun chronometrist-details ()
  "Display details of time tracked over a period of time."
  (interactive)
  (let* ((buffer (get-buffer-create (chronometrist-details-buffer-name)))
         (window (save-excursion
                   (get-buffer-window buffer t))))
    (cond (window (kill-buffer buffer))
          (t (chronometrist-details-setup-buffer buffer)))))
;; chronometrist-details:1 ends here

;; [[file:chronometrist.org::*range][range:1]]
(defvar chronometrist-details-range nil
  "Time range for intervals displayed by `chronometrist-details'.
Values can be one of -
nil - no range. Display all intervals for today.
An ISO date string - display intervals for this date.
A cons cell in the form (BEGIN . END), where BEGIN and END are
ISO date strings (inclusive) or date-time strings (\"BEGIN\"
inclusive, \"END\" exclusive) - display intervals in this
range.")
(make-variable-buffer-local 'chronometrist-details-range)
;; range:1 ends here

;; [[file:chronometrist.org::*iso-date-p][iso-date-p:1]]
(defun chronometrist-iso-date-p (string)
  "Return non-nil if STRING is a date in the ISO-8601 format."
  (string-match-p
   (rx (and string-start
            (>= 1 num) "-" (= 2 num) "-" (= 2 num)
            string-end))
   string))
;; iso-date-p:1 ends here

;; [[file:chronometrist.org::*intervals-for-range][intervals-for-range:1]]
(defun chronometrist-details-intervals-for-range (range table)
  "Return intervals for RANGE from TABLE.
RANGE must be a time range as specified by `chronometrist-details-range'.

TABLE must be a hash table as returned by
`chronometrist-to-hash-table'."
  (pcase range
    ('nil
     (gethash (format-time-string "%F") table))
    ((pred stringp)
     (gethash range table))
    (`(,begin . ,end)
     ;; `chronometrist-iso-to-ts' also accepts ISO dates
     (let ((begin-ts (chronometrist-iso-to-ts begin))
           (end-ts   (chronometrist-iso-to-ts end)))
       (if (and (chronometrist-iso-date-p begin) (chronometrist-iso-date-p end))
           (cl-loop while (not (ts> begin-ts end-ts))
             append (gethash (ts-format "%F" begin-ts) table)
             do (ts-adjustf begin-ts 'day 1))
         (cl-loop while (not (ts> begin-ts end-ts))
           append
           (cl-loop for plist in (gethash (ts-format "%F" begin-ts) table)
             when
             (let ((start-ts (chronometrist-iso-to-ts (plist-get plist :start)))
                   (stop-ts  (chronometrist-iso-to-ts (plist-get plist :stop))))
               (and (ts>= start-ts begin-ts)
                    (ts<= stop-ts end-ts)))
             collect plist)
           do (ts-adjustf begin-ts 'day 1)))))))

;; (chronometrist-details-intervals-for-range nil chronometrist-events)
;; (chronometrist-details-intervals-for-range "2021-06-01" chronometrist-events)
;; (chronometrist-details-intervals-for-range '("2021-06-01" . "2021-06-03") chronometrist-events)
;; (chronometrist-details-intervals-for-range '("2021-06-02T01:00+05:30" . "2021-06-02T03:00+05:30") chronometrist-events)
;; intervals-for-range:1 ends here

;; [[file:chronometrist.org::*input-to-value][input-to-value:1]]
(defun chronometrist-details-input-to-value (input)
  "Return INPUT as a value acceptable to `chronometrist-details-range'."
  (pcase input
    ('nil nil)
    (`(,date) date)
    (`(,begin ,end)
     (let* ((ht-keys      (hash-table-keys
                           (chronometrist-backend-hash-table (chronometrist-active-backend))))
            (date-p       (seq-find #'chronometrist-iso-date-p input))
            (begin-date   (car ht-keys))
            (begin-iso-ts (ts-format
                           "%FT%T%z" (chronometrist-iso-to-ts begin-date)))
            (end-date     (car (last ht-keys)))
            (end-iso-ts   (chronometrist-format-time-iso8601))
            (begin (if (equal begin "begin")
                       (if date-p begin-date begin-iso-ts)
                     begin))
            (end   (if (equal end "end")
                       (if date-p end-date end-iso-ts)
                     end)))
       (cons begin end)))
    (_ (error "Unsupported range %S" input))))
;; input-to-value:1 ends here

;; [[file:chronometrist.org::*set-range][set-range:1]]
(defun chronometrist-details-set-range ()
  "Prompt user for range for current `chronometrist-details' buffer."
  (interactive)
  (let* ((hash-table (chronometrist-backend-hash-table (chronometrist-active-backend)))
         (input (completing-read-multiple
                 (concat "Range (blank, ISO-8601 date, "
                         "or two ISO-8601 dates/timestamps): ")
                 (append '("begin" "end")
                         (reverse (hash-table-keys hash-table)))
                 nil nil (pcase chronometrist-details-range
                           ('nil nil)
                           ((pred stringp)
                            (format "%s" chronometrist-details-range))
                           (`(,begin . ,end)
                            (format "%s,%s" begin end)))
                 'chronometrist-details-range-history))
         (new-value (chronometrist-details-input-to-value input))
         (buffer-name (pcase new-value
                        (`(,begin . ,end)
                         (chronometrist-details-buffer-name (format "%s_%s" begin end)))
                        ((pred stringp)
                         (chronometrist-details-buffer-name new-value)))))
    (chronometrist-details-setup-buffer (get-buffer-create buffer-name))
    (with-current-buffer buffer-name
      (setq-local chronometrist-details-range new-value)
      (tabulated-list-revert))))
;; set-range:1 ends here

;; [[file:chronometrist.org::*filter][filter:1]]
(defvar chronometrist-details-filter nil
  "Parameters to filter intervals displayed by `chronometrist-details'.
Values can be one of -
nil - no filter. Display all intervals in the given time range.
A list of keywords - display intervals containing all given keywords.
A plist - display intervals containing all given keyword-values.
A predicate of one argument (the interval plist) - display all
intervals for which the predicate returns non-nil.")
(make-variable-buffer-local 'chronometrist-details-filter)
;; filter:1 ends here

;; [[file:chronometrist.org::*filter-match-p][filter-match-p:1]]
(defun chronometrist-details-filter-match-p (plist filter)
  "Return PLIST if it matches FILTER.
FILTER must be a filter specifier as described by
`chronometrist-details-filter'."
  (cond ((null filter) plist)
        ((seq-every-p #'keywordp filter)
         (when (--every-p (plist-get plist it) filter)
           plist))
        ((chronometrist-plist-p filter)
         (when (cl-loop for (keyword value) on filter by #'cddr
                 always (equal (plist-get plist keyword) value))
           plist))
        ((functionp filter)
         (when (funcall filter plist) plist))
        (t (error "Invalid filter %S" filter))))
;; filter-match-p:1 ends here

;; [[file:chronometrist.org::*set-filter][set-filter:1]]
(defun chronometrist-details-set-filter ()
  "Prompt user for filter for current `chronometrist-details' buffer."
  (interactive)
  (let* ((input (read-from-minibuffer
                 (concat "Filter (blank, a list of keywords, "
                         "a plist, or a predicate): ")
                 nil nil nil 'chronometrist-details-filter-history
                 (pcase chronometrist-details-filter
                   ('nil "")
                   ((pred consp) (format "%S" chronometrist-details-filter)))))
         (sexp (ignore-errors (read input))))
    (cond ((equal input "") (setq-local chronometrist-details-filter nil))
          ((consp sexp)     (setq-local chronometrist-details-filter sexp))
          (t (error "Unsupported filter %S" input)))
    (tabulated-list-revert)))
;; set-filter:1 ends here

;; [[file:chronometrist.org::*intervals][intervals:1]]
(defun chronometrist-details-intervals (range filter backend)
  "Return plists matching RANGE and FILTER from BACKEND.
For values of RANGE, see `chronometrist-details-range'. For
values of FILTER, see `chronometrist-details-filter'. TABLE must
be a hash table as returned by `chronometrist-to-hash-table'."
  (cl-loop for plist in (chronometrist-details-intervals-for-range range (chronometrist-backend-hash-table backend))
    when (chronometrist-details-filter-match-p plist filter)
    collect plist))
;; intervals:1 ends here

;; [[file:chronometrist.org::*rows][rows:1]]
(defun chronometrist-details-rows ()
  "Return rows to be displayed in the `chronometrist-details' buffer.
Return value is a list as specified by `tabulated-list-entries'."
  (cl-loop with index = 1
    for plist in (chronometrist-details-intervals chronometrist-details-range chronometrist-details-filter (chronometrist-active-backend))
    collect
    (-let* (((&plist :name name :tags tags :start start :stop stop) plist)
            ;; whether tags or key-values are actually displayed is handled later
            (tags       (chronometrist-details-rows-helper tags))
            (key-values (chronometrist-details-rows-helper plist))
            ;; resetting seconds with `ts-apply' is necessary to
            ;; prevent situations like "1 hour  from 00:08 to 01:09"
            (start   (ts-apply :second 0 (chronometrist-iso-to-ts start)))
            (stop    (ts-apply :second 0 (if stop
                                             (chronometrist-iso-to-ts stop)
                                           (ts-now))))
            (interval      (floor (ts-diff stop start)))
            (index-string  (format "%s" index))
            (duration      (chronometrist-format-duration-long interval))
            (timespan (format "from %s to %s"
                              (ts-format chronometrist-details-time-format-string
                                         start)
                              (ts-format chronometrist-details-time-format-string
                                         stop))))
      (--> (vconcat (vector index-string name)
                    (when chronometrist-details-display-tags (vector tags))
                    (when chronometrist-details-display-key-values (vector key-values))
                    (vector duration timespan))
        (list index it)
        (chronometrist-run-transformers chronometrist-details-row-transformers it)))
    do (cl-incf index)))
;; rows:1 ends here

(provide 'chronometrist)

;;; chronometrist.el ends here
