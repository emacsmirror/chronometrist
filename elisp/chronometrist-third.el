;; [[file:chronometrist-third.org::*fraction][fraction:1]]
(defcustom chronometrist-third-divisor 3
  "Number to determine accumulation of break time relative to work time.")
;; fraction:1 ends here

;; [[file:chronometrist-third.org::*break-time][break-time:1]]
(defvar chronometrist-third-break-time 0
  "Accumulated break time in seconds.")
;; break-time:1 ends here

;; [[file:chronometrist-third.org::*clock-in][clock-in:1]]
(defun chronometrist-third-clock-in ()
  (let ((latest-break-duration ))
    ))

;; (chronometrist-interval (chronometrist-latest-record (chronometrist-active-backend)))
;; clock-in:1 ends here

;; [[file:chronometrist-third.org::*clock-out][clock-out:1]]
(defun chronometrist-third-clock-out ()
  (let ((latest-work-duration ))))
;; clock-out:1 ends here
