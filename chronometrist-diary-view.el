(defvar chronometrist-diary-buffer-name "*Chronometrist-Diary*")

(defun chronometrist-intervals-on (date)
  "Return a list of all time intervals on DATE, where DATE is a
list in the form (YEAR MONTH DAY). Each time interval is in the
format returned by `encode-time'."
  (->> (gethash date chronometrist-events)
       (chronometrist-events->time-list)
       (-partition 2)
       (--map (time-subtract (cadr it)
                             (car it)))
       (--map (chronometrist-seconds-to-hms (cadr it)))))

;; "X minutes on PROJECT (REASON)"

(defun chronometrist-diary-projects-reasons-on (date)
  "Return a list of projects and reasons on DATE."
  (->> (gethash date chronometrist-events)
       (seq-map #'chronometrist-vlast)
       (-partition 2)
       (mapcar (lambda (elt)
                 (let ((project (car elt))
                       (reason  (cadr elt)))
                   (concat " on " project
                           (unless (equal reason "")
                             (concat " (" reason ")"))
                           "\n"))))))

(defun chronometrist-decode-time->date (date)
  (->> (decode-time)
       (-take 6)
       (reverse)
       (-take 3)))

(defun chronometrist-diary-refresh (&optional ignore-auto noconfirm date)
  (let* ((date (if date
                   date
                 (chronometrist-decode-time->date (decode-time))))
         (intervals (->> date
                         (chronometrist-intervals-on)
                         (mapcar #'chronometrist-format-time)))
         (projects-reasons (chronometrist-diary-projects-reasons-on date))
         (inhibit-read-only t))
    (chronometrist-common-clear-buffer chronometrist-diary-buffer-name)
    (seq-mapn #'insert intervals projects-reasons)))

(define-derived-mode chronometrist-diary-view-mode special-mode "Chronometrist Diary"
  "A mode to view your activity today like a diary."
  (setq revert-buffer-function #'chronometrist-diary-refresh))

(defun chronometrist-diary-view (&optional date)
  (interactive)
  (switch-to-buffer
   (get-buffer-create chronometrist-diary-buffer-name))
  (chronometrist-diary-view-mode)
  (chronometrist-diary-refresh))

;; Local Variables:
;; nameless-current-name: "chronometrist-diary"
;; End:
