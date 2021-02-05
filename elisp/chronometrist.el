;;; chronometrist.el --- A time tracker with a nice interface -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://github.com/contrapunctus-1/chronometrist
;; Package-Requires: ((emacs "25.1")
;;                    (dash "2.16.0")
;;                    (seq "2.20")
;;                    (s "1.12.0")
;;                    (ts "0.2")
;;                    (anaphora "1.0.4"))
;; Version: 0.6.4

;;; Commentary:
;;
;; A time tracker in Emacs with a nice interface

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

;; For information on usage and customization, see https://github.com/contrapunctus-1/chronometrist/blob/master/README.md

;;; Code:
(require 'filenotify)
(require 'cl-lib)
(require 'subr-x)
(require 'chronometrist-key-values)
(require 'chronometrist-queries)
(require 'chronometrist-migrate)
(require 'chronometrist-sexp)

(require 'dash)
(require 'cl-lib)
(require 'ts)
(require 'chronometrist-time)

(eval-when-compile
  (defvar chronometrist-mode-map)
  (require 'subr-x))
(autoload 'chronometrist-maybe-start-timer "chronometrist-timer" nil t)
(autoload 'chronometrist-report "chronometrist-report" nil t)
(autoload 'chronometrist-statistics "chronometrist-statistics" nil t)

(defcustom chronometrist-sexp-pretty-print-function #'chronometrist-plist-pp
  "Function used to pretty print plists in `chronometrist-file'.
Like `pp', it must accept an OBJECT and optionally a
STREAM (which is the value of `current-buffer')."
  :type 'function)

(define-derived-mode chronometrist-sexp-mode
  ;; fundamental-mode
  emacs-lisp-mode
  "chronometrist-sexp")

(defmacro chronometrist-sexp-in-file (file &rest body)
  "Run BODY in a buffer visiting FILE, restoring point afterwards."
  (declare (indent defun) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion ,@body)))

(defmacro chronometrist-loop-file (for expr in file &rest loop-clauses)
  "`cl-loop' LOOP-CLAUSES over s-expressions in FILE, in reverse.
VAR is bound to each s-expression."
  (declare (indent defun)
           (debug nil)
           ;; FIXME
           ;; (debug ("for" form "in" form &rest &or sexp form))
           )
  `(chronometrist-sexp-in-file ,file
     (goto-char (point-max))
     (cl-loop with ,expr
       while (and (not (bobp))
                  (backward-list)
                  (or (not (bobp))
                      (not (looking-at-p "^[[:blank:]]*;")))
                  (setq ,expr (ignore-errors (read (current-buffer))))
                  (backward-list))
       ,@loop-clauses)))

;;;; Queries
(defun chronometrist-sexp-open-log ()
  "Open `chronometrist-file' in another window."
  (find-file-other-window chronometrist-file)
  (goto-char (point-max)))

(defun chronometrist-sexp-last ()
  "Return last s-expression from `chronometrist-file'."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-max))
    (backward-list)
    (ignore-errors (read (current-buffer)))))

(defun chronometrist-sexp-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (let ((last-event (chronometrist-sexp-last)))
    (if (plist-member last-event :stop)
        nil
      (plist-get last-event :name))))

(defun chronometrist-sexp-events-populate ()
  "Populate hash table `chronometrist-events'.
The data is acquired from `chronometrist-file'.

Return final number of events read from file, or nil if there
were none."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-min))
    (let ((index 0) expr pending-expr)
      (while (or pending-expr
                 (setq expr (ignore-errors (read (current-buffer)))))
        ;; find and split midnight-spanning events during deserialization itself
        (let* ((split-expr (chronometrist-events-maybe-split expr))
               (new-value  (cond (pending-expr
                                  (prog1 pending-expr
                                    (setq pending-expr nil)))
                                 (split-expr
                                  (setq pending-expr (cl-second split-expr))
                                  (cl-first split-expr))
                                 (t expr)))
               (new-value-date (->> (plist-get new-value :start)
                                    (s-left 10)))
               (existing-value (gethash new-value-date chronometrist-events)))
          (unless pending-expr (cl-incf index))
          (puthash new-value-date
                   (if existing-value
                       (append existing-value
                               (list new-value))
                     (list new-value))
                   chronometrist-events)))
      (unless (zerop index) index))))

;;;; Modifications
(defun chronometrist-sexp-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (unless (file-exists-p chronometrist-file)
    (with-current-buffer (find-file-noselect chronometrist-file)
      (goto-char (point-min))
      (insert ";;; -*- mode: chronometrist-sexp; -*-")
      (write-file chronometrist-file))))

(cl-defun chronometrist-sexp-new (plist)
  "Add new PLIST at the end of `chronometrist-file'."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-max))
    ;; If we're adding the first s-exp in the file, don't add a
    ;; newline before it
    (unless (bobp) (insert "\n"))
    (unless (bolp) (insert "\n"))
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (save-buffer)))

(defun chronometrist-sexp-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(defun chronometrist-sexp-replace-last (plist)
  "Replace the last s-expression in `chronometrist-file' with PLIST."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-max))
    (unless (and (bobp) (bolp)) (insert "\n"))
    (backward-list 1)
    (chronometrist-sexp-delete-list)
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (save-buffer)))

(defun chronometrist-sexp-reindent-buffer ()
  "Reindent the current buffer.
This is meant to be run in `chronometrist-file' when using the s-expression backend."
  (interactive)
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

(defvar chronometrist-events (make-hash-table :test #'equal)
  "Each key is a date in the form (YEAR MONTH DAY).
Values are lists containing events, where each event is a list in
the form (:name \"NAME\" :tags (TAGS) <key value pairs> ...
:start TIME :stop TIME).")

(defun chronometrist-day-start (timestamp)
  "Get start of day (according to `chronometrist-day-start-time') for TIMESTAMP.
TIMESTAMP must be a time string in the ISO-8601 format.

Return value is a time value (see `current-time')."
  (let ((timestamp-date-list (->> timestamp
                                  (parse-iso8601-time-string)
                                  (decode-time)
                                  (-drop 3)
                                  (-take 3))))
    (--> chronometrist-day-start-time
         (split-string it ":")
         (mapcar #'string-to-number it)
         (reverse it)
         (append it timestamp-date-list)
         (apply #'encode-time it))))

(defun chronometrist-events-maybe-split (event)
  "Split EVENT if it spans midnight.
Return a list of two events if EVENT was split, else nil."
  (when (plist-get event :stop)
    (let ((split-time (chronometrist-midnight-spanning-p (plist-get event :start)
                                             (plist-get event :stop))))
      (when split-time
        (let ((first-start  (plist-get (cl-first  split-time) :start))
              (first-stop   (plist-get (cl-first  split-time) :stop))
              (second-start (plist-get (cl-second split-time) :start))
              (second-stop  (plist-get (cl-second split-time) :stop))
              ;; plist-put modifies lists in-place. The resulting bugs
              ;; left me puzzled for a while.
              (event-1      (cl-copy-list event))
              (event-2      (cl-copy-list event)))
          (list (-> event-1
                    (plist-put :start first-start)
                    (plist-put :stop  first-stop))
                (-> event-2
                    (plist-put :start second-start)
                    (plist-put :stop  second-stop))))))))

(defun chronometrist-events-populate ()
  "Clear hash table `chronometrist-events' (which see) and populate it.
The data is acquired from `chronometrist-file'.

Return final number of events read from file, or nil if there
were none."
  (clrhash chronometrist-events)
  (chronometrist-sexp-events-populate))

(defun chronometrist-events-update (plist &optional replace)
  "Add PLIST to the end of `chronometrist-events'.
If REPLACE is non-nil, replace the last event with PLIST."
  (let* ((date (->> (plist-get plist :start)
                    (chronometrist-iso-timestamp->ts )
                    (ts-format "%F" )))
         (events-today (gethash date chronometrist-events)))
    (--> (if replace (-drop-last 1 events-today) events-today)
         (append it (list plist))
         (puthash date it chronometrist-events))))

(defun chronometrist-events-subset (start end)
  "Return a subset of `chronometrist-events'.
The subset will contain values between dates START and END (both
inclusive).

START and END must be ts structs (see `ts.el'). They will be
treated as though their time is 00:00:00."
  (let ((subset (make-hash-table :test #'equal))
        (start  (chronometrist-date start))
        (end    (chronometrist-date end)))
    (maphash (lambda (key value)
               (when (ts-in start end (chronometrist-iso-date->ts key))
                 (puthash key value subset)))
             chronometrist-events)
    subset))

(defgroup chronometrist nil
  "A time tracker with a nice UI."
  :group 'applications)

(defcustom chronometrist-file
  (locate-user-emacs-file "chronometrist.sexp")
  "Default path and name of the Chronometrist database.

It should be a text file containing plists in the form -
\(:name \"task name\"
 [:tags TAGS]
 [:comment \"comment\"]
 [KEY-VALUE-PAIR ...]
 :start \"TIME\"
 :stop \"TIME\"\)

Where -

TAGS is a list. It can contain any strings and symbols.

KEY-VALUE-PAIR can be any keyword-value pairs. Currently,
Chronometrist ignores them.

TIME must be an ISO-8601 time string.

\(The square brackets here refer to optional elements, not
vectors.\)"
  :type 'file)

(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'."
  :type 'string)

(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer."
  :type 'boolean)

(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\")."
  :type 'integer)

(defcustom chronometrist-activity-indicator "*"
  "How to indicate that a task is active.
Can be a string to be displayed, or a function which returns this string.
The default is \"*\""
  :type '(choice string function))

(defcustom chronometrist-day-start-time "00:00:00"
  "The time at which a day is considered to start, in \"HH:MM:SS\".

The default is midnight, i.e. \"00:00:00\"."
  :type 'string)

(defvar chronometrist--point nil)

(defun chronometrist-open-log (&optional _button)
  "Open `chronometrist-file' in another window.

Argument _BUTTON is for the purpose of using this command as a
button action."
  (interactive)
  (chronometrist-sexp-open-log))

(defun chronometrist-common-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (chronometrist-sexp-create-file))

(defun chronometrist-task-active? (task)
  "Return t if TASK is currently clocked in, else nil."
  (equal (chronometrist-current-task) task))

(defun chronometrist-activity-indicator ()
  "Return a string to indicate that a task is active.
See custom variable `chronometrist-activity-indicator'."
  (if (functionp chronometrist-activity-indicator)
      (funcall chronometrist-activity-indicator)
    chronometrist-activity-indicator))

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

(defun chronometrist-entries ()
  "Create entries to be displayed in the buffer created by `chronometrist', in the format specified by `tabulated-list-entries'."
  ;; HACK - these calls are commented out, because `chronometrist-entries' is
  ;; called by both `chronometrist-refresh' and `chronometrist-refresh-file', and only the
  ;; latter should refresh from a file.
  ;; (chronometrist-events-populate)
  ;; (chronometrist-events-clean)
  (->> (-sort #'string-lessp chronometrist-task-list)
       (--map-indexed
        (let* ((task        it)
               (index       (number-to-string (1+ it-index)))
               (task-button `(,task action chronometrist-toggle-task-button follow-link t))
               (task-time   (chronometrist-format-time (chronometrist-task-time-one-day task)))
               (indicator   (if (chronometrist-task-active? task) (chronometrist-activity-indicator) "")))
          (--> (vector index task-button task-time indicator)
               (list task it)
               (chronometrist-run-transformers chronometrist-entry-transformers it))))))

(defun chronometrist-task-at-point ()
  "Return the task at point in the `chronometrist' buffer, or nil if there is no task at point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "[0-9]+ +" nil t)
      (get-text-property (point) 'tabulated-list-id))))

(defun chronometrist-goto-last-task ()
  "In the `chronometrist' buffer, move point to the line containing the last active task."
  (goto-char (point-min))
  (re-search-forward (plist-get (chronometrist-last) :name) nil t)
  (beginning-of-line))

(defun chronometrist-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert
   (format "\n% 18s - %s"
           (chronometrist-format-keybinds command chronometrist-mode-map firstonly)
           (if description description ""))))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (with-current-buffer chronometrist-buffer-name
    (let ((inhibit-read-only t)
          (w "\n    ")
          ;; (keybind-start-new (chronometrist-format-keybinds 'chronometrist-add-new-task chronometrist-mode-map))
          (keybind-toggle    (chronometrist-format-keybinds 'chronometrist-toggle-task chronometrist-mode-map t)))
      (goto-char (point-max))
      (--> (chronometrist-active-time-one-day)
           (chronometrist-format-time it)
           (format "%s%- 26s%s" w "Total" it)
           (insert it))
      (insert "\n")
      (insert w (format "% 17s" "Keys") w (format "% 17s" "----"))
      (chronometrist-print-keybind 'chronometrist-add-new-task)
      (insert-text-button "start a new task" 'action #'chronometrist-add-new-task-button 'follow-link t)
      (chronometrist-print-keybind 'chronometrist-toggle-task "toggle task at point")
      (chronometrist-print-keybind 'chronometrist-toggle-task-no-hooks "toggle without running hooks")
      (insert "\n " (format "%s %s - %s" "<numeric argument N>" keybind-toggle "toggle <N>th task"))
      (chronometrist-print-keybind 'chronometrist-report)
      (insert-text-button "see weekly report" 'action #'chronometrist-report 'follow-link t)
      (chronometrist-print-keybind 'chronometrist-open-log)
      (insert-text-button "view/edit log file" 'action #'chronometrist-open-log 'follow-link t)
      (insert "\n"))))

(defun chronometrist-goto-nth-task (n)
  "Move point to the line containing the Nth task.
Return the task at point, or nil if there is no corresponding
task. N must be a positive integer."
  (goto-char (point-min))
  (when (re-search-forward (format "^%d" n) nil t)
    (beginning-of-line)
    (chronometrist-task-at-point)))

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

(defvar chronometrist--file-state nil
  "List containing the state of `chronometrist-file'.
`chronometrist-refresh-file' sets this to a plist in the form

\(:last (LAST-START LAST-END) :rest (REST-START REST-END HASH))

\(see `chronometrist-file-hash')

LAST-START and LAST-END represent the start and the end of the
last s-expression.

REST-START and REST-END represent the start of the file and the
end of the second-last s-expression.")

(defun chronometrist-file-hash (&optional start end hash)
  "Calculate hash of `chronometrist-file' between START and END.
START can be
a number or marker,
:before-last - the position at the start of the last s-expression
nil or any other value - the value of `point-min'.

END can be
a number or marker,
:before-last - the position at the end of the second-last s-expression,
nil or any other value - the position at the end of the last s-expression.

Return (START END) if HASH is nil, else (START END HASH).

Return a list in the form (A B HASH), where A and B are markers
in `chronometrist-file' describing the region for which HASH was calculated."
  (chronometrist-sexp-in-file chronometrist-file
    (let* ((start (cond ((number-or-marker-p start) start)
                        ((eq :before-last start)
                         (goto-char (point-max))
                         (backward-list))
                        (t (point-min))))
           (end   (cond ((number-or-marker-p end) end)
                        ((eq :before-last end)
                         (goto-char (point-max))
                         (backward-list 2)
                         (forward-list))
                        (t (goto-char (point-max))
                           (backward-list)
                           (forward-list)))))
      (if hash
          (--> (buffer-substring-no-properties start end)
               (secure-hash 'sha1 it)
               (list start end it))
        (list start end)))))

(defun chronometrist-read-from (position)
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char
     (if (number-or-marker-p position)
         position
       (funcall position)))
    (ignore-errors (read (current-buffer)))))

(defun chronometrist-file-change-type (state)
  "Determine the type of change made to `chronometrist-file'.
STATE must be a plist. (see `chronometrist--file-state')

Return
:append  if a new s-expression was added to the end,
:modify  if the last s-expression was modified,
:remove  if the last s-expression was removed,
    nil  if the contents didn't change, and
      t  for any other change."
  (-let* (((last-start last-end)           (plist-get state :last))
          ((rest-start rest-end rest-hash) (plist-get state :rest))
          ;; Using a hash to determine if the last expression has
          ;; changed can cause issues - the expression may shrink, and
          ;; if we try to compute the hash of the old region again, we
          ;; will get an args-out-of-range error. A hash will also
          ;; result in false negatives for whitespace/indentation
          ;; differences.
          (last-same-p     (--> (hash-table-keys chronometrist-events) (last it) (car it)
                                (gethash it chronometrist-events) (last it) (car it)
                                (equal it (chronometrist-read-from last-start))))
          (file-new-length (chronometrist-sexp-in-file chronometrist-file (point-max)))
          (rest-same-p (unless (< file-new-length rest-end)
                         (equal rest-hash
                                (cl-third (chronometrist-file-hash rest-start rest-end t))))))
    (cond ((not rest-same-p) t)
          (last-same-p
           (when (chronometrist-read-from last-end) :append))
          ((not (chronometrist-read-from last-start))
           :remove)
          ((not (chronometrist-read-from
                 (lambda ()
                   (progn (goto-char last-start)
                          (forward-list)))))
           :modify))))

(defun chronometrist-task-list ()
  "Return a list of tasks from `chronometrist-file'."
  (--> (chronometrist-loop-file for plist in chronometrist-file collect (plist-get plist :name))
       (cl-remove-duplicates it :test #'equal)
       (sort it #'string-lessp)))

(defun chronometrist-add-to-task-list (task)
  (unless (cl-member task chronometrist-task-list :test #'equal)
    (setq chronometrist-task-list
          (sort (cons task chronometrist-task-list) #'string-lessp))))

(defun chronometrist-remove-from-task-list (task)
  (let ((count (cl-loop with count = 0
                 for intervals being the hash-values of chronometrist-events
                 do (cl-loop for interval in intervals
                      do (cl-incf count))
                 finally return count))
        (position (cl-loop with count = 0
                    for intervals being the hash-values of chronometrist-events
                    when (cl-loop for interval in intervals
                           do (cl-incf count)
                           when (equal task (plist-get interval :name))
                           return t)
                    return count)))
    (when (and position (= position count))
      ;; The only interval for TASK is the last expression
      (setq chronometrist-task-list (remove task chronometrist-task-list)))))

(defun chronometrist-refresh-file (fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist' buffer.
Argument _FS-EVENT is ignored."
  (run-hooks 'chronometrist-file-change-hook)
  ;; (message "chronometrist - file %s" fs-event)
  ;; `chronometrist-file-change-type' must be run /before/ we update `chronometrist--file-state'
  ;; (the latter represents the old state of the file, which
  ;; `chronometrist-file-change-type' compares with the new one)
  (-let* (((descriptor action file ...) fs-event)
          (change      (when chronometrist--file-state
                         (chronometrist-file-change-type chronometrist--file-state)))
          (reset-watch (or (eq action 'deleted) (eq action 'renamed))))
    ;; (message "chronometrist - file change type is %s" change)
    (cond ((or reset-watch (not chronometrist--file-state) (eq change t))
           (when reset-watch
             (file-notify-rm-watch chronometrist--fs-watch)
             (setq chronometrist--fs-watch nil chronometrist--file-state nil))
           (chronometrist-events-populate)
           (setq chronometrist-task-list (chronometrist-task-list)))
          (chronometrist--file-state
           (let ((task (plist-get (chronometrist-last) :name)))
             (pcase change
               (:append
                (chronometrist-events-update (chronometrist-sexp-last))
                (chronometrist-add-to-task-list task))
               (:modify
                (chronometrist-events-update (chronometrist-sexp-last) t)
                (chronometrist-remove-from-task-list task)
                (chronometrist-add-to-task-list task))
               (:remove
                (let* ((date (--> (hash-table-keys chronometrist-events)
                                  (last it)
                                  (car it)))
                       (old-task (--> (gethash date chronometrist-events)
                                      (last it)
                                      (car it)
                                      (plist-get it :name))))
                  (chronometrist-remove-from-task-list old-task)
                  (--> (gethash date chronometrist-events)
                       (-drop-last 1 it)
                       (puthash date it chronometrist-events))))
               ((pred null) nil)))))
    (setq chronometrist--file-state
          (list :last (chronometrist-file-hash :before-last nil)
                :rest (chronometrist-file-hash nil :before-last t)))
    ;; REVIEW - can we move most/all of this to the `chronometrist-file-change-hook'?
    (chronometrist-refresh)))

(defun chronometrist-query-stop ()
  "Ask the user if they would like to clock out."
  (let ((task (chronometrist-current-task)))
    (and task
         (yes-or-no-p (format "Stop tracking time for %s? " task))
         (chronometrist-out))
    t))

(defun chronometrist-in (task &optional _prefix)
  "Clock in to TASK; record current time in `chronometrist-file'.
TASK is the name of the task, a string. PREFIX is ignored."
  (interactive "P")
  (let ((plist `(:name ,task :start ,(chronometrist-format-time-iso8601))))
    (chronometrist-sexp-new plist)
    (chronometrist-refresh)))

(defun chronometrist-out (&optional _prefix)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.
PREFIX is ignored."
  (interactive "P")
  (let ((plist (plist-put (chronometrist-last) :stop (chronometrist-format-time-iso8601))))
    (chronometrist-sexp-replace-last plist)))

(defvar chronometrist-mode-hook nil
  "Normal hook run at the very end of `chronometrist-mode'.")

(defvar chronometrist-list-format-transformers nil
  "List of functions to transform `tabulated-list-format' (which see).
This is called with `chronometrist-run-transformers' in `chronometrist-mode', which see.

Extensions using `chronometrist-list-format-transformers' to
increase the number of columns will also need to modify the value
of `tabulated-list-entries' by using
`chronometrist-entry-transformers'.")

(defvar chronometrist-entry-transformers nil
  "List of functions to transform each entry of `tabulated-list-entries'.
This is called with `chronometrist-run-transformers' in `chronometrist-entries', which see.

Extensions using `chronometrist-entry-transformers' to increase
the number of columns will also need to modify the value of
`tabulated-list-format' by using
`chronometrist-list-format-transformers'.")

(defvar chronometrist-before-in-functions nil
  "Functions to run before a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook.")

(defvar chronometrist-after-in-functions nil
  "Functions to run after a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook.")

(defvar chronometrist-before-out-functions nil
  "Functions to run before a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.

The task will be stopped only if all functions in this list
return a non-nil value.")

(defvar chronometrist-after-out-functions nil
  "Functions to run after a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.")

(defvar chronometrist-file-change-hook nil
  "Functions to be run after `chronometrist-file' is changed on disk.")

(defun chronometrist-run-functions-and-clock-in (task)
  "Run hooks and clock in to TASK."
  (run-hook-with-args 'chronometrist-before-in-functions task)
  (chronometrist-in task)
  (run-hook-with-args 'chronometrist-after-in-functions task))

(defun chronometrist-run-functions-and-clock-out (task)
  "Run hooks and clock out of TASK."
  (when (run-hook-with-args-until-failure 'chronometrist-before-out-functions task)
    (chronometrist-out)
    (run-hook-with-args 'chronometrist-after-out-functions task)))

(defvar chronometrist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'chronometrist-toggle-task)
    (define-key map (kbd "M-RET") #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "l")     #'chronometrist-open-log)
    (define-key map (kbd "r")     #'chronometrist-report)
    (define-key map [mouse-1]     #'chronometrist-toggle-task)
    (define-key map [mouse-3]     #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "a")     #'chronometrist-add-new-task)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (make-local-variable 'tabulated-list-format)
  (--> [("#" 3 t) ("Task" 25 t) ("Time" 10 t) ("Active" 10 t)]
        (chronometrist-run-transformers chronometrist-list-format-transformers it)
        (setq tabulated-list-format it))
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function #'chronometrist-refresh)
  (run-hooks 'chronometrist-mode-hook))

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

(defun chronometrist-add-new-task-button (_button)
  "Button action to add a new task.

Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (let ((current (chronometrist-current-task)))
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (let ((task (read-from-minibuffer "New task name: " nil nil nil nil nil t)))
      (chronometrist-run-functions-and-clock-in task))))

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
  (let* ((empty-file   (chronometrist-common-file-empty-p chronometrist-file))
         (nth          (when prefix (chronometrist-goto-nth-task prefix)))
         (at-point     (chronometrist-task-at-point))
         (target       (or nth at-point))
         (current      (chronometrist-current-task))
         (in-function  (if inhibit-hooks
                           #'chronometrist-in
                         #'chronometrist-run-functions-and-clock-in))
         (out-function (if inhibit-hooks
                           #'chronometrist-out
                         #'chronometrist-run-functions-and-clock-out)))
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

(defun chronometrist-toggle-task-no-hooks (&optional prefix)
  "Like `chronometrist-toggle-task', but don't run hooks.

With numeric prefix argument PREFIX, toggle the Nth task. If there
is no corresponding task, do nothing."
  (interactive "P")
  (chronometrist-toggle-task prefix t))

(defun chronometrist-add-new-task ()
  "Add a new task."
  (interactive)
  (chronometrist-add-new-task-button nil))

;;;###autoload
(defun chronometrist (&optional arg)
  "Display the user's tasks and the time spent on them today.

Based on their timelog file `chronometrist-file'. This is the
'listing command' for `chronometrist-mode'.

If numeric argument ARG is 1, run `chronometrist-report'.
If numeric argument ARG is 2, run `chronometrist-statistics'."
  (interactive "P")
  (chronometrist-migrate-check)
  (let ((buffer (get-buffer-create chronometrist-buffer-name))
        (w      (save-excursion
                  (get-buffer-window chronometrist-buffer-name t))))
    (cond
     (arg (cl-case arg
            (1 (chronometrist-report))
            (2 (chronometrist-statistics))))
     (w (with-current-buffer buffer
          (setq chronometrist--point (point))
          (kill-buffer chronometrist-buffer-name)))
     (t (with-current-buffer buffer
          (cond ((or (not (file-exists-p chronometrist-file))
                     (chronometrist-common-file-empty-p chronometrist-file))
                 ;; first run
                 (chronometrist-common-create-file)
                 (let ((inhibit-read-only t))
                   (chronometrist-common-clear-buffer buffer)
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
                   (if (hash-table-keys chronometrist-events)
                       (chronometrist-refresh)
                     (chronometrist-refresh-file nil))
                   (if chronometrist--point
                       (goto-char chronometrist--point)
                     (chronometrist-goto-last-task))))
          (unless chronometrist--fs-watch
            (setq chronometrist--fs-watch
                  (file-notify-add-watch chronometrist-file '(change) #'chronometrist-refresh-file))))))))

(defvar chronometrist-task-list nil
  "List of tasks in `chronometrist-file'.")

(defvar chronometrist--fs-watch nil
  "Filesystem watch object.
Used to prevent more than one watch being added for the same
file.")

(defun chronometrist-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (chronometrist-sexp-current-task))

(cl-defun chronometrist-format-time (seconds &optional (blank "   "))
  "Format SECONDS as a string suitable for display in Chronometrist buffers.
SECONDS must be a positive integer.

BLANK is a string to display in place of blank values. If not
supplied, 3 spaces are used."
  (-let [(h m s) (chronometrist-seconds-to-hms seconds)]
    (if (and (zerop h) (zerop m) (zerop s))
        "       -"
      (let ((h (if (zerop h)
                   blank
                 (format "%2d:" h)))
            (m (cond ((and (zerop h)
                           (zerop m))
                      blank)
                     ((zerop h)
                      (format "%2d:" m))
                     (t
                      (format "%02d:" m))))
            (s (if (and (zerop h)
                        (zerop m))
                   (format "%2d" s)
                 (format "%02d" s))))
        (concat h m s)))))

(defun chronometrist-common-file-empty-p (file)
  "Return t if FILE is empty."
  (let ((size (elt (file-attributes file) 7)))
    (if (zerop size) t nil)))

(defun chronometrist-common-clear-buffer (buffer)
  "Clear the contents of BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (delete-region (point-min) (point-max))))

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

(defun chronometrist-events->ts-pairs (events)
  "Convert EVENTS to a list of ts struct pairs (see `ts.el').

EVENTS must be a list of valid Chronometrist property lists (see
`chronometrist-file')."
  (cl-loop for plist in events collect
           (let* ((start (chronometrist-iso-timestamp->ts
                          (plist-get plist :start)))
                  (stop (plist-get plist :stop))
                  (stop (if stop
                            (chronometrist-iso-timestamp->ts stop)
                          (ts-now))))
             (cons start stop))))

(defun chronometrist-ts-pairs->durations (ts-pairs)
  "Return the durations represented by TS-PAIRS.
TS-PAIRS is a list of pairs, where each element is a ts struct (see `ts.el').

Return seconds as an integer, or 0 if TS-PAIRS is nil."
  (if ts-pairs
      (cl-loop for pair in ts-pairs collect
               (ts-diff (cdr pair) (car pair)))
    0))

(defun chronometrist-previous-week-start (ts)
  "Find the previous `chronometrist-report-week-start-day' from TS.

Return a ts struct for said day's beginning.

If the day of TS is the same as the
`chronometrist-report-week-start-day', return TS.

TS must be a ts struct (see `ts.el')."
  (cl-loop
    with week-start =
    (alist-get chronometrist-report-week-start-day chronometrist-report-weekday-number-alist nil nil #'equal)
    until (= week-start (ts-dow ts))
    do (ts-decf (ts-day ts))
    finally return ts))

(provide 'chronometrist)
