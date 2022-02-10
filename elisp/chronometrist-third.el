;; [[file:chronometrist-third.org::*divisor][divisor:1]]
(defcustom chronometrist-third-divisor 3
  "Number to determine accumulation of break time relative to work time.")
;; divisor:1 ends here

;; [[file:chronometrist-third.org::*break-time][break-time:1]]
(defvar chronometrist-third-break-time 0
  "Accumulated break time in seconds.")
;; break-time:1 ends here

;; [[file:chronometrist-third.org::*clock-in][clock-in:1]]
(defun chronometrist-third-clock-in ()
  (unless (zerop chronometrist-third-break-time)
    (-let* (((&plist :stop stop) (cl-second (chronometrist-to-list (chronometrist-active-backend))))
            (used-break-duration (ts-diff (ts-now) (chronometrist-iso-to-ts stop)))
            (new-break-time      (- chronometrist-third-break-time used-break-duration)))
      (setq chronometrist-third-break-time
            (if (> new-break-time 0)
                new-break-time
              0)))))
;; clock-in:1 ends here

;; [[file:chronometrist-third.org::*clock-out][clock-out:1]]
(defun chronometrist-third-clock-out ()
  (let ((latest-work-duration (chronometrist-interval (chronometrist-latest-record (chronometrist-active-backend)))))
    (cl-incf chronometrist-third-break-time (/ latest-work-duration chronometrist-third-divisor))
    ;; start notification timer(s)
    ;; ...
    ))
;; clock-out:1 ends here
