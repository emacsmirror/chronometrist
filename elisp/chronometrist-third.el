;; [[file:chronometrist-third.org::*divisor][divisor:1]]
(defcustom chronometrist-third-divisor 3
  "Number to determine accumulation of break time relative to work time.")
;; divisor:1 ends here

;; [[file:chronometrist-third.org::*break-time][break-time:1]]
(defvar chronometrist-third-break-time 0
  "Accumulated break time in seconds.")
;; break-time:1 ends here

;; [[file:chronometrist-third.org::*alert-functions][alert-functions:1]]
(defcustom chronometrist-third-alert-functions
  '(chronometrist-half-alert chronometrist-quarter-alert)
  "List of timed alerts for the Third Time system.

Typically, each function in this list should call `run-at-time'
to run another function, which in turn should call `alert' to
notify the user."
  :group 'chronometrist-third
  :type 'hook)
;; alert-functions:1 ends here

;; [[file:chronometrist-third.org::*alert-functions][alert-functions:2]]
(defun chronometrist-third-half-alert ()
  (and (not (zerop chronometrist-third-break-time))
       (run-at-time )))
;; alert-functions:2 ends here

;; [[file:chronometrist-third.org::*clock-in][clock-in:1]]
(defun chronometrist-third-clock-in (&optional arg)
  ;; stop alert timer
  ;; ...
  ;; update break-time
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
(defun chronometrist-third-clock-out (&optional arg)
  (let ((latest-work-duration (chronometrist-interval (chronometrist-latest-record (chronometrist-active-backend)))))
    (cl-incf chronometrist-third-break-time (/ latest-work-duration chronometrist-third-divisor))
    ;; start alert timer(s)
    ;; ...
    ))
;; clock-out:1 ends here

;; [[file:chronometrist-third.org::*third-minor-mode][third-minor-mode:1]]
(define-minor-mode chronometrist-third-minor-mode
  nil nil nil nil
  (cond (chronometrist-third-minor-mode
         (add-hook 'chronometrist-after-in-functions #'chronometrist-third-clock-in)
         (add-hook 'chronometrist-after-out-functions #'chronometrist-third-clock-out))
        (t (remove-hook 'chronometrist-after-in-functions #'chronometrist-third-clock-in)
           (remove-hook 'chronometrist-after-out-functions #'chronometrist-third-clock-out))))
;; third-minor-mode:1 ends here
