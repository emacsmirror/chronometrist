;;; chronometrist-time.el --- Time and date functions for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

(require 'parse-time)
(require 'dash)
(require 's)

;;; Commentary:
;; Pretty sure quite a few of these are redundant. Hopefully putting
;; them together in the same file will make it easier to figure out
;; which ones those are.

;;; Code:

(defconst chronometrist-seconds-in-day (* 60 60 24)
  "Number of seconds in a day.")

(defun chronometrist-date (&optional time)
  "Return date for TIME or today, in the form \"YYYY-MM-DD\"."
  (s-left 10 (chronometrist-format-time-iso8601 time)))

;; (defun chronometrist-time (&optional time))

(defun chronometrist-day-of-week->number (day-of-week)
  "Return an integer (0-6) representing DAY-OF-WEEK.

DAY-OF-WEEK should be a string, e.g. \"Sunday\" - see
`chronometrist-report-weekday-number-alist'."
  (cdr
   (assoc-string day-of-week chronometrist-report-weekday-number-alist)))

(defun chronometrist-number->day-of-week (number)
  "Return the day of the week (as a string), corresponding to NUMBER.

NUMBER should be an integer (0-6) - see
`chronometrist-report-weekday-number-alist'."
  (car
   (rassoc number chronometrist-report-weekday-number-alist)))

(defun chronometrist-format-time-iso8601 (&optional unix-time)
  "Return current moment as an ISO-8601 format time string.

Optional argument UNIX-TIME should be a time value (see
`current-time') accepted by `format-time-string'."
  (format-time-string "%FT%T%z" unix-time))

(defun chronometrist-time-interval-span-midnight? (t1 t2)
  "Return t if time range T1 to T2 extends across midnight.

T1 and T2 must be lists in the form (YEAR MONTH DAY HOURS MINUTES
SECONDS), as returned by `timestamp->list'. T2 must be
chronologically more recent than T1."
  (let* ((day-1   (elt t1 2))
         (day-2   (elt t2 2))
         (month-1 (elt t1 1))
         (month-2 (elt t2 1)))
    ;; not Absolutely Perfect™, but should do for most situations
    (or (= day-2   (1+ day-1))
        (= month-2 (1+ month-1)))))

;; Note - this assumes that an event never crosses >1 day. This seems
;; sufficient for all conceivable cases.
(defun chronometrist-midnight-spanning-p (start-time stop-time)
  "Return non-nil if START-TIME and STOP-TIME cross a midnight.

Return value is a list in the form
\((:start START-TIME
  :stop <day-start time on initial day>)
 (:start <day start time on second day>
  :stop STOP-TIME))"
  ;; FIXME - time zones are ignored; may cause issues with
  ;; time-zone-spanning events

  ;; The time on which the first provided day starts (according to `chronometrist-day-start-time')
  (let* ((first-day-start (chronometrist-day-start start-time))
         ;; HACK - won't work with custom day-start time
         ;; (first-day-end   (parse-iso8601-time-string
         ;;                   (concat (chronometrist-date (parse-iso8601-time-string start-time))
         ;;                           "24:00:00")))
         (next-day-start  (time-add first-day-start
                                    '(0 . 86400)))
         (stop-time-unix  (parse-iso8601-time-string stop-time)))
    ;; Does the event stop time exceed the the next day start time?
    (when (time-less-p next-day-start stop-time-unix)
      (list `(:start ,start-time
                     :stop  ,(chronometrist-format-time-iso8601 next-day-start))
            `(:start ,(chronometrist-format-time-iso8601 next-day-start)
                     :stop  ,stop-time)))))

(defun chronometrist-timestamp->list (date-time-string)
  "Convert string timestamp DATE-TIME-STRING to a list of integers."
  (--> date-time-string
       (split-string it "[-/ :]")
       (mapcar #'string-to-number it)))

(defun chronometrist-timestamp-list->seconds (date-time-list)
  "Convert DATE-TIME-LIST to seconds since the UNIX epoch.
DATE-TIME-LIST must be a list in the form (YEAR MONTH DAY HOURS
MINUTES SECONDS), as returned by `timestamp->list'.

See (info \"(elisp)Time of Day\")."
  (declare (obsolete nil "Chronometrist v0.3.0"))
  (->> date-time-list
       (reverse)
       (apply #'encode-time)))

(defun chronometrist-timestamp->seconds (date-time-string)
  "Convert DATE-TIME-STRING to seconds since the UNIX epoch.
DATE-TIME-STRING must be a string in the form \"YYYY/MM/SS HH:MM:SS\".

See (info \"(elisp)Time of Day\")."
  (declare (obsolete nil "Chronometrist v0.3.0"))
  (chronometrist-timestamp-list->seconds
   (chronometrist-timestamp->list date-time-string)))

(defun chronometrist-time->seconds (duration)
  "Convert DURATION to seconds.
DURATION must be a vector in the form [HOURS MINUTES SECONDS]."
  (-let [[h m s] duration]
    (+ (* h 60 60)
       (* m 60)
       s)))

(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES SECONDS].
SECONDS must be a positive integer."
  (let* ((seconds (truncate seconds))
         (s       (% seconds 60))
         (m       (% (/ seconds 60) 60))
         (h       (/ seconds 3600)))
    (vector h m s)))

(defun chronometrist-time-add (a b)
  "Add durations A and B and return a vector in the same form.
A and B should be vectors in the form [HOURS MINUTES SECONDS]."
  (let ((h1 (elt a 0))
        (m1 (elt a 1))
        (s1 (elt a 2))
        (h2 (elt b 0))
        (m2 (elt b 1))
        (s2 (elt b 2)))
    (chronometrist-seconds-to-hms (+ (* h1 3600) (* h2 3600)
                        (* m1 60) (* m2 60)
                        s1 s2))))

(defun chronometrist-iso-date->timestamp (date)
  "Convert DATE to a complete timestamp by adding a time part (T00:00:00)."
  ;; potential problem - time zones are ignored
  (concat date "T00:00:00"))

(defun chronometrist-date->time (date)
  "Convert DATE to a time value (see (info \"(elisp)Time of Day\")).
DATE must be a list in the form (YEAR MONTH DAY)."
  (->> date (reverse) (apply #'encode-time 0 0 0)))

(defun chronometrist-date-less-p (date1 date2)
  "Like `time-less-p' but for dates. Return t if DATE1 is less than DATE2.
Both must be dates in the ISO-8601 format."
  (time-less-p (-> date1
                   (chronometrist-iso-date->timestamp)
                   (parse-iso8601-time-string))
               (-> date2
                   (chronometrist-iso-date->timestamp)
                   (parse-iso8601-time-string))))

(defun chronometrist-time-less-or-equal-p (t1 t2)
  "Return t if T1 is less than or equal to T2.

T1 and T2 should be time values (see `current-time')."
  (or (equal t1 t2)
      (time-less-p t1 t2)))

(defun chronometrist-calendrical->date (date)
  "Convert calendrical information DATE to a date in the form (YEAR MONTH DAY).

For input format, see (info \"(elisp)Time of Day\")."
  (-> date (-slice 3 6) (reverse)))

(defun chronometrist-interval (event)
  "Return the period of time covered by EVENT as a time value.
EVENT should be a plist (see `chronometrist-file')."
  (let ((start (plist-get event :start))
        (stop  (plist-get event :stop)))
    (time-subtract (parse-iso8601-time-string stop)
                   (parse-iso8601-time-string start))))

(provide 'chronometrist-time)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-time.el ends here
