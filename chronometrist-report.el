;;; chronometrist-report.el --- Report view for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;;; Commentary:
;;

(require 'chronometrist-common)
(require 'chronometrist-timer)
(require 'chronometrist-report-custom)
(require 'chronometrist-migrate)

;; TODO - improve first-run (no file, or no data in file) behaviour

;; TODO - add support for custom week start day to
;; tabulated-list-format. Have it use chronometrist-report-weekday-number-alist for day
;; names to aid i10n

;; TODO - use variables instead of hardcoded numbers to determine spacing

;; ## VARIABLES ##

;;; Code:

(defvar chronometrist-report--ui-date nil
  "The first date of the week displayed by `chronometrist-report'.
A value of nil means the current week. Otherwise, it must be a
date in the form \"YYYY-MM-DD\".")

(defvar chronometrist-report--ui-week-dates nil
  "List of dates currently displayed by `chronometrist-report'.
Each date is a list containing calendrical information (see (info \"(elisp)Time Conversion\"))")

(defvar chronometrist-report--point nil)

;; ## FUNCTIONS ##

(defun chronometrist-report-date ()
  "Return the date specified by `chronometrist-report--ui-date'.
If it is nil, return the current date as calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (if chronometrist-report--ui-date chronometrist-report--ui-date (chronometrist-date)))

(defun chronometrist-report-date->dates-in-week (first-date-in-week)
  "Return a list in the form (DAY-1 DAY-2 ... DAY-7), where each
day is a time value (see (info \"(elisp)Time of Day\")).

FIRST-DATE-IN-WEEK must be a time value representing DAY-1."
  (--> '(0 1 2 3 4 5 6)
       ;; 1 day = 86400 seconds
       (--map (* 86400 it) it)
       (--map (list
               (car first-date-in-week)
               (+ (cadr first-date-in-week) it))
              it)))

(defun chronometrist-report-date->week-dates ()
  "Return dates in week as a list, where each element is
calendrical information (see (info \"(elisp)Time Conversion\")).
The first date is the first occurrence of
`chronometrist-report-week-start-day' before the date specified in
`chronometrist-report--ui-date' (if non-nil) or the current date."
  (->> (or chronometrist-report--ui-date (chronometrist-date))
       (chronometrist-previous-week-start)
       (chronometrist-report-date->dates-in-week)))

(defun chronometrist-report-entries ()
  "Creates entries to be displayed in the buffer created by
`chronometrist-report'."
  (let* ((week-dates        (chronometrist-report-date->week-dates))) ;; uses today if chronometrist-report--ui-date is nil
    (setq chronometrist-report--ui-week-dates week-dates)
    (mapcar (lambda (project)
              (let ((project-daily-time-list
                     (--map (chronometrist-task-time-one-day project
                                                 (chronometrist-date it))
                            week-dates)))
                (list project
                      (vconcat
                       (vector project)
                       (->> project-daily-time-list
                            (mapcar #'chronometrist-format-time)
                            (apply #'vector))
                       (->> project-daily-time-list
                            (-reduce #'chronometrist-time-add)
                            (chronometrist-format-time)
                            (vector))))))
            chronometrist-task-list)))

(defun chronometrist-report-format-date (format-string time-date)
  "Extract date from TIME-DATE and format it according to
FORMAT-STRING."
  (->> time-date
       (-take 6)
       (-drop 3)
       (reverse)
       (apply #'format format-string)))

(defun chronometrist-report-print-keybind (command &optional description firstonly)
  (insert "\n    "
          (chronometrist-format-keybinds command firstonly)
          " - "
          (if description description "")))

;; TODO - preserve point when clicking buttons
(defun chronometrist-report-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-report'."
  (let ((inhibit-read-only t)
        (w            "\n    ")
        (key-previous (chronometrist-format-keybinds #'chronometrist-report-previous-week
                                         chronometrist-report-mode-map
                                         t))
        (key-next     (chronometrist-format-keybinds #'chronometrist-report-next-week
                                         chronometrist-report-mode-map
                                         t)))
    (goto-char (point-min))
    (insert "                         ")
    (--map (insert (chronometrist-date it) " ")
           (chronometrist-report-date->week-dates))
    (insert "\n")
    (goto-char (point-max))
    (insert w (format "%- 21s" "Total"))
    (let ((total-time-daily (->> chronometrist-report--ui-week-dates
                                 (mapcar #'chronometrist-date)
                                 (mapcar #'chronometrist-active-time-one-day))))
      (->> total-time-daily
           (mapcar #'chronometrist-format-time)
           (--map (format "% 9s  " it))
           (apply #'insert))
      (->> total-time-daily
           (-reduce #'chronometrist-time-add)
           (chronometrist-format-time)
           (format "% 13s")
           (insert)))

    (insert "\n" w)
    (insert-text-button "<<"
                        'action #'chronometrist-report-previous-week
                        'follow-link t)
    (insert (format "% 4s" " "))
    (insert-text-button ">>"
                        'action #'chronometrist-report-next-week
                        'follow-link t)

    (insert "\n")
    (chronometrist-report-print-keybind 'chronometrist-report-previous-week)
    (insert-text-button "previous week"
                        'action #'chronometrist-report-previous-week
                        'follow-link t)
    (chronometrist-report-print-keybind 'chronometrist-report-next-week)
    (insert-text-button "next week"
                        'action #'chronometrist-report-next-week
                        'follow-link t)
    (chronometrist-report-print-keybind 'chronometrist-open-file)
    (insert-text-button "open log file"
                        'action #'chronometrist-open-file
                        'follow-link t)))

(defun chronometrist-report-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist-report' buffer, without re-reading `chronometrist-file'."
  (let* ((w (get-buffer-window chronometrist-report-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-report-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-report-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))

;; REVIEW - merge this into `chronometrist-refresh-file', while moving the -refresh call to the call site?
(defun chronometrist-report-refresh-file (_fs-event)
  "Re-populate and clean `chronometrist-events', and refresh the `chronometrist-report' buffer.
Argument _FS-EVENT is ignored."
  (chronometrist-events-populate)
  ;; (chronometrist-events-clean)
  (chronometrist-report-refresh))

;; ## MAJOR MODE ##

(defvar chronometrist-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-file)
    (define-key map (kbd "b") #'chronometrist-report-previous-week)
    (define-key map (kbd "f") #'chronometrist-report-next-week)
    ;; Works when number of projects < screen length; after that, you
    ;; probably expect mousewheel to scroll up/down, and
    ;; alt-mousewheel or something for next/previous week. For now,
    ;; I'm assuming most people won't have all that many tasks - I've
    ;; been using it for ~2 months and have 18 projects, which are
    ;; still just half the screen on my 15" laptop. Let's see what
    ;; people say.
    (define-key map [mouse-4] #'chronometrist-report-next-week)
    (define-key map [mouse-5] #'chronometrist-report-previous-week)
    map)
  "Keymap used by `chronometrist-report-mode'.")

(define-derived-mode chronometrist-report-mode tabulated-list-mode "Chronometrist-Report"
  "Major mode for `chronometrist-report'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Project"   25 t)
                               ("Sunday"    10 t)
                               ("Monday"    10 t)
                               ("Tuesday"   10 t)
                               ("Wednesday" 10 t)
                               ("Thursday"  10 t)
                               ("Friday"    10 t)
                               ("Saturday"  10 t :pad-right 5)
                               ("Total"     12 t)])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-report-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))
  (tabulated-list-init-header)
  (chronometrist-maybe-start-timer)
  (setq revert-buffer-function #'chronometrist-report-refresh)
  (unless chronometrist--fs-watch
    (setq chronometrist--fs-watch
          (file-notify-add-watch chronometrist-file
                                 '(change)
                                 #'chronometrist-refresh-file))))

;; ## COMMANDS ##

;;;###autoload
(defun chronometrist-report (&optional keep-date)
  "Display a weekly report of the data in `chronometrist-file'.

 This is the 'listing command' for ‘chronometrist-report-mode’.

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
            (t (delete-other-windows)
               (when (not keep-date)
                 (setq chronometrist-report--ui-date nil))
               (chronometrist-common-create-chronometrist-file)
               (chronometrist-report-mode)
               (switch-to-buffer buffer)
               (chronometrist-report-refresh-file nil)
               (goto-char (or chronometrist-report--point 1)))))))

(defun chronometrist-report-previous-week (arg)
  "View the previous week's report."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (setq chronometrist-report--ui-date
          (thread-first (if chronometrist-report--ui-date
                            (parse-iso8601-time-string
                             (chronometrist-iso-date->timestamp chronometrist-report--ui-date))
                          (current-time))
            (time-subtract `(0 ,(* 7 arg 86400)))
            (chronometrist-date))))
  (setq chronometrist-report--point (point))
  (kill-buffer)
  (chronometrist-report t))

(defun chronometrist-report-next-week (arg)
  "View the next week's report."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (setq chronometrist-report--ui-date
          (thread-first (if chronometrist-report--ui-date
                            (parse-iso8601-time-string
                             (chronometrist-iso-date->timestamp chronometrist-report--ui-date))
                          (current-time))
            (time-add `(0 ,(* 7 arg 86400)))
            (chronometrist-date)))
    (setq chronometrist-report--point (point))
    (kill-buffer)
    (chronometrist-report t)))

(provide 'chronometrist-report)

;; Local Variables:
;; nameless-current-name: "chronometrist-report"
;; End:

;;; chronometrist-report.el ends here
