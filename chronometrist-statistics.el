;;; chronometrist-statistics.el --- View statistics from timeclock.el data

;;; Commentary:
;;

(require 'chronometrist-common)
(require 'chronometrist-timer)
(require 'chronometrist-events)
(require 'chronometrist-statistics-custom)

;; for each activity -
;; [x] days active - int (float percent)
;;     - get days in range - count days on which worked on activity
;; [x] average time - HH:MM:SS
;;     - get days in range - get time spent per day for activity in range - get the length and the total -> (/ (+ durations) length)
;; [ ] current streak - [int years, int months,] int days
;;     - get days in range - calculate all streaks - see if last streak is on current day
;; [ ] last streak    - [int years, int months,] int days
;;     - get days in range - calculate all streaks - if current streak, return second-last streak, else return last streak
;; [ ] longest streak - [int years, int months,] int days
;;     - get days in range - calculate all streaks - find longest streak
;; [ ] days since last active - int
;; ...where "range" is a week, a month, a year, the entire range of records, or an arbitrary date range

;; details!
;; for each activity, spent most time on doing X (where X is a
;; comment, assuming you use comments to detail what you did)

;; Really might need emacs-async for this...buttloads of big
;; calculations which will only get bigger as the timelog file grows,
;; and the more the activities, the more the calculations! I'm
;; visualizing the table loading gradually, field by field, like an
;; image in a browser.

;; TODO -
;; 1. [x] show dash instead of zero
;; 2. [x] percent for active days
;; 3. buttons
;; 4. [x] display date ranges in a nicer way
;; 5. month and year ranges
;; 6. totals for each column
;; 7. (maybe) jump between chronometrist-report and chronometrist-statistics for viewing the same week's data
;;    - in chronometrist-statistics, this only makes sense in week mode
;; 8. a 'counter' - if I have ten weeks of data and I'm on the latest,
;;    show 10/10; update this as we scroll
;;    - don't scroll past the end, as currently happens
;;    - also applicable to chronometrist-report

;; TODO - convert all functions which take dates as arguments to use
;; the (YEAR MONTH DAY) format

;;; Code:

(defun chronometrist-date->time (date)
  "Convert DATE to a time value (see (info \"(elisp)Time of Day\")).
DATE must be a list in the form (YEAR MONTH DAY)."
  (->> date (reverse) (apply #'encode-time 0 0 0)))

(defun chronometrist-date-less-p (date1 date2)
  "Like `time-less-p' but for dates. Return t if DATE1 is less than DATE2.
Both must be lists in the form (YEAR MONTH DAY)."
  (time-less-p (chronometrist-date->time date1) (chronometrist-date->time date2)))

(defun chronometrist-statistics-count-active-days (project &optional table)
  "Return the number of days the user spent any time on PROJECT.
TABLE must be a hash table - if not supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((count 0)
        (table (if table table chronometrist-events)))
    (maphash (lambda (date events)
               (when (seq-find (lambda (event)
                                 (and (equal (elt event 0) "i")
                                      (equal (elt event 7) project)))
                               events)
                 (setq count (1+ count))))
             table)
    count))

(defun chronometrist-project-events-in-day (project date)
  "Get events for PROJECT on DATE. DATE must be in the form (YEAR MONTH DAY).

Returns a list of events, where each event is a vector in the
form [CODE YEAR MONTH DAY HOURS MINUTES SECONDS PROJECT-OR-COMMENT].

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((save-next)
        (results))
    (seq-do (lambda (event)
              (cond ((and (equal "i" (chronometrist-vfirst event))
                          (equal project (chronometrist-vlast event)))
                     (->> event (list) (append results) (setq results))
                     (setq save-next t))
                    (save-next
                     (->> event (list) (append results) (setq results))
                     (setq save-next nil))
                    (t nil)))
            (gethash date chronometrist-events))
    results))

(defun chronometrist-events->time-list (events)
  "Convert EVENTS to a list of time values.

EVENTS must be a list of vectors in the form [\"CODE\" YEAR MONTH
DATE HOUR MINUTES SECONDS \"PROJECT-NAME-OR-COMMENT\"] (see
`chronometrist-events' and `chronometrist-events-populate')

For time value format, see (info \"(elisp)Time of Day\")."
  (--map (pcase it
           (`[,_ ,year ,month ,day ,h ,m ,s ,_]
            (encode-time s m h day month year)))
         events))

(defun chronometrist-time-list->sum-of-intervals (time-values)
  "From a list of start/end timestamps TIME-VALUES, get the total time interval.

TIME-VALUES are a list of time values (see (info \"(elisp)Time of
Day\")), assumed to be alternating start/end times. This function
obtains the intervals between them, and adds the intervals to
return a single time value.

If TIME-VALUES is nil, return '(0 0)."
  (if time-values
      (->> (-partition 2 time-values)
           (--map (time-subtract (cadr it) (car it)))
           (-reduce #'time-add))
    '(0 0)))

(defun chronometrist-statistics-count-average-time-spent (project &optional table)
  "Return the average time the user has spent on PROJECT from TABLE.

TABLE must be a hash table - if not supplied,
`chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((table (if table table chronometrist-events))
        (days  0)
        (per-day-time-list))
    (maphash (lambda (key value)
               (let ((events-in-day (chronometrist-project-events-in-day project key)))
                 (when events-in-day
                   (setq days (1+ days))
                   (->> events-in-day
                        (chronometrist-events->time-list)
                        (chronometrist-time-list->sum-of-intervals)
                        (list)
                        (append per-day-time-list)
                        (setq per-day-time-list)))))
             table)
    (if per-day-time-list
        (--> per-day-time-list
             (-reduce #'time-add it)
             (cadr it)
             (/ it days))
      0)))

;; ## VARIABLES ##

(defvar chronometrist-statistics--ui-state nil
  "Stores the display state for `chronometrist-statistics'.

This must be a plist in the form (:MODE :START :END).

:MODE is either 'week, 'month, 'year, 'full, or 'custom. 'week,
'month, and 'year mean display statistics weekly/monthly/yearly
respectively.

'full means display statistics from the beginning to the end of
the `timeclock-file'.

'custom means display statistics from an arbitrary date range.

:START and :END are the start and end of the date range to be
displayed. They must be dates in the form (YEAR MONTH DAY).")

(defvar chronometrist-statistics--point nil)

;; ## FUNCTIONS ##

(defun chronometrist-calendrical->date (date)
  "Convert calendrical information DATE to a date in the form (YEAR MONTH DAY).

For input format, see (info \"(elisp)Time of Day\")."
  (-> date (-slice 3 6) (reverse)))

(defun chronometrist-statistics-entries-internal (table)
  "Helper function for `chronometrist-statistics-entries'.

It simply operates on the entire hash table TABLE (see
`chronometrist-events' for table format), so ensure that TABLE is
reduced to the desired range using
`chronometrist-events-subset'."
  (mapcar (lambda (project)
            (let* ((active-days    (chronometrist-statistics-count-active-days project table))
                   (active-percent (case (plist-get chronometrist-statistics--ui-state :mode)
                                     ('week (* 100 (/ active-days 7.0)))))
                   (active-percent (if (zerop active-days)
                                       (format "    % 6s" "-")
                                     (format "    %05.2f%%" active-percent)))
                   (active-days    (format "% 5s"
                                           (if (zerop active-days)
                                               "-"
                                             active-days)))
                   (average-time   (->> (chronometrist-statistics-count-average-time-spent project table)
                                        (chronometrist-seconds-to-hms)
                                        (chronometrist-format-time)
                                        (format "% 5s")))
                   (content        (vector project
                                           active-days
                                           active-percent
                                           average-time)))
              (list project content)))
          timeclock-project-list))

(defun chronometrist-statistics-entries ()
  "Create entries to be displayed in the buffer created by `chronometrist-statistics'."
  ;; We assume that all fields in `chronometrist-statistics--ui-state' are set, so they must
  ;; be changed by the view-changing functions.
  (case (plist-get chronometrist-statistics--ui-state :mode)
    ('week
     (let* ((start (plist-get chronometrist-statistics--ui-state :start))
            (end   (plist-get chronometrist-statistics--ui-state :end))
            (table (chronometrist-events-subset start end)))
       (chronometrist-statistics-entries-internal table)))
    (t ;; `chronometrist-statistics--ui-state' is nil, show current week's data
     (let* ((start-long (chronometrist-report-previous-week-start (decode-time)))
            (end-long   (chronometrist-date-op start-long '+ 7))
            (start      (chronometrist-calendrical->date start-long))
            (end        (chronometrist-calendrical->date end-long))
            (table      (chronometrist-events-subset start end)))
       (setq chronometrist-statistics--ui-state `(:mode week :start ,start :end ,end))
       (chronometrist-statistics-entries-internal table)))))

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

(defun chronometrist-statistics-format-date (date)
  "Return DATE (YEAR MONTH DAY) as a string in the form \"YYYY-MM-DD\"."
  (-let [(year month day) date]
    (format "%04d-%02d-%02d" year month day)))

(defun chronometrist-statistics-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-statistics'."
  (let ((w "\n    ")
        (inhibit-read-only t))
    (goto-char (point-max))
    (insert w)
    (insert-text-button (case (plist-get chronometrist-statistics--ui-state :mode)
                          ('week "Weekly view"))
                        ;; 'action #'chronometrist-report-previous-week ;; TODO - make interactive function to accept new mode from user
                        'follow-link t)
    (insert ", from")
    (insert
     (format " %s to %s\n"
             (chronometrist-statistics-format-date (plist-get chronometrist-statistics--ui-state :start))
             (chronometrist-statistics-format-date (plist-get chronometrist-statistics--ui-state :end))))))

(defun chronometrist-statistics-refresh (&optional ignore-auto noconfirm)
  "Refresh the `chronometrist-statistics' buffer, without re-reading `timeclock-file'.

The optional arguments IGNORE-AUTO and NOCONFIRM are ignored, and
are present solely for the sake of using this function as a value
of `revert-buffer-function'."
  (let* ((w (get-buffer-window chronometrist-statistics-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-statistics-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-statistics-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))

;; ## MAJOR MODE ##

(defvar chronometrist-statistics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-timeclock-file)
    (define-key map (kbd "b") #'chronometrist-statistics-previous-range)
    (define-key map (kbd "f") #'chronometrist-statistics-next-range)
    map)
  "Keymap used by `chronometrist-statistics-mode'.")

(define-derived-mode chronometrist-statistics-mode tabulated-list-mode "Chronometrist-Statistics"
  "Major mode for `chronometrist-statistics'."
  (timeclock-reread-log)
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        [("Project"      25 t)
         ("Active days"  12 t)
         ("%% of days active"   17 t)
         ("Average time" 12 t)
         ;; ("Current streak"           10 t)
         ;; ("Last streak"              10 t)
         ;; ("Longest streak"           10 t)
         ])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-statistics-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))
  (tabulated-list-init-header)
  ;; (chronometrist-maybe-start-timer)
  (setq revert-buffer-function #'chronometrist-statistics-refresh)
  (file-notify-add-watch timeclock-file '(change) #'chronometrist-refresh-file))

;; ## COMMANDS ##

;;;###autoload
(defun chronometrist-statistics (&optional preserve-state)
  "Display statistics based on data from the user's `timeclock-file'.
This is the 'listing command' for `chronometrist-statistics-mode'.

If a buffer called `chronometrist-statistics-buffer-name' already
exists and is visible, kill the buffer.

If PRESERVE-STATE is nil (the default when not supplied), display
data from the current week. Otherwise, display data from the week
specified by `chronometrist-statistics--ui-state'."
  (interactive)
  (let* ((buffer (get-buffer-create chronometrist-statistics-buffer-name))
         (today  (decode-time))
         (week-start (chronometrist-calendrical->date
                      (chronometrist-report-previous-week-start today)))
         (week-end   (chronometrist-date-op week-start #'+ 6)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
             (when (not preserve-state)
               (setq chronometrist-statistics--ui-state `(:mode week
                                  :start ,week-start
                                  :end ,week-end)))
             (chronometrist-common-create-timeclock-file)
             (chronometrist-statistics-mode)
             (switch-to-buffer buffer)
             (chronometrist-statistics-refresh))))))

(defun chronometrist-statistics-previous-range (arg)
  "View the statistics in the previous time range.

If ARG is a numeric argument, go back that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start))
         (end   (plist-get chronometrist-statistics--ui-state :end)))
    (case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (chronometrist-date-op start     #'- (* 7 arg)))
              (new-end   (chronometrist-date-op new-start #'+ 6)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(defun chronometrist-statistics-next-range (arg)
  "View the statistics in the next time range.

If ARG is a numeric argument, go forward that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start))
         (end   (plist-get chronometrist-statistics--ui-state :end)))
    (case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (chronometrist-date-op start     #'+ (* 7 arg)))
              (new-end   (chronometrist-date-op new-start #'+ 6)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

;; Local Variables:
;; nameless-current-name: "chronometrist-statistics"
;; End:

(provide 'chronometrist-statistics)

;;; chronometrist-statistics.el ends here
