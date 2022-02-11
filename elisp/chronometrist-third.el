;;; chronometrist-third.el --- Third Time support for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "25.1") (alert "1.2") (chronometrist "0.6.0"))
;; Version: 0.0.1

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;; Add support for the Third Time system to Chronometrist.  In Third
;; Time, you work for any length of time you like, and "earn" a third
;; of the work time as break time.  For a more detailed explanation,
;; see
;; https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work

;; For information on usage and customization, see https://tildegit.org/contrapunctus/chronometrist-goal/src/branch/production/README.md

;;; Code:
(require 'chronometrist)
(require 'alert)

;; [[file:chronometrist-third.org::*group][group:1]]
(defgroup chronometrist-third nil
  "Third Time support for Chronometrist."
  :group 'chronometrist)
;; group:1 ends here

;; [[file:chronometrist-third.org::*divisor][divisor:1]]
(defcustom chronometrist-third-divisor 3
  "Number to determine accumulation of break time relative to work time."
  :type 'number)
;; divisor:1 ends here

;; [[file:chronometrist-third.org::*duration-format][duration-format:1]]
(defcustom chronometrist-third-duration-format "%H, %M, and %S%z"
  "Format string for durations, passed to `format-seconds'."
  :type 'string)
;; duration-format:1 ends here

;; [[file:chronometrist-third.org::*break-time][break-time:1]]
(defvar chronometrist-third-break-time 0
  "Accumulated break time in seconds.")
;; break-time:1 ends here

;; [[file:chronometrist-third.org::*alert-functions][alert-functions:1]]
(defcustom chronometrist-third-alert-functions '(chronometrist-third-half-alert chronometrist-third-quarter-alert chronometrist-third-break-over-alert)
  "List of timed alerts for the Third Time system.

Typically, each function in this list should call
`chronometrist-third-run-at-time' to run another function, which
in turn should call `alert' to notify the user.

All functions in this list are started when the user clocks out,
and stopped when they clock in."
  :group 'chronometrist-third
  :type 'hook)
;; alert-functions:1 ends here

;; [[file:chronometrist-third.org::*timer-list][timer-list:1]]
(defvar chronometrist-third-timer-list nil)
;; timer-list:1 ends here

;; [[file:chronometrist-third.org::*run-at-time][run-at-time:1]]
(defun chronometrist-third-run-at-time (time repeat function &rest args)
  "Like `run-at-time', but store timer objects in `chronometrist-third-timer-list'."
  (cl-pushnew (apply #'run-at-time time repeat function args) chronometrist-third-timer-list))
;; run-at-time:1 ends here

;; [[file:chronometrist-third.org::*half-alert][half-alert:1]]
(defun chronometrist-third-half-alert ()
  "Display an alert when half the break time is consumed."
  (let ((half-time (/ chronometrist-third-break-time 2.0)))
    (and (not (zerop chronometrist-third-break-time))
         (chronometrist-third-run-at-time
          half-time nil
          (lambda ()
            (alert
             (format "%s left on your break."
                     (format-seconds chronometrist-third-duration-format half-time))))))))
;; half-alert:1 ends here

;; [[file:chronometrist-third.org::*quarter-alert][quarter-alert:1]]
(defun chronometrist-third-quarter-alert ()
  "Display an alert when 3/4ths of the break time is consumed."
  (let ((three-fourths (* chronometrist-third-break-time 7.5)))
    (and (not (zerop chronometrist-third-break-time))
         (chronometrist-third-run-at-time
          three-fourths nil
          (lambda ()
            (alert
             (format "%s left on your break."
                     (format-seconds chronometrist-third-duration-format
                                     (- chronometrist-third-break-time three-fourths)))))))))
;; quarter-alert:1 ends here

;; [[file:chronometrist-third.org::*break-over-alert][break-over-alert:1]]
(defun chronometrist-third-break-over-alert ()
  "Display an alert when break time is over."
  (and (not (zerop chronometrist-third-break-time))
       (chronometrist-third-run-at-time
        chronometrist-third-break-time nil
        (lambda () (alert (format "Break time is over!"))))))
;; break-over-alert:1 ends here

;; [[file:chronometrist-third.org::*start-alert-timers][start-alert-timers:1]]
(defun chronometrist-third-start-alert-timers ()
  "Run functions in `chronometrist-third-alert-functions'."
  (mapc #'funcall chronometrist-third-alert-functions))
;; start-alert-timers:1 ends here

;; [[file:chronometrist-third.org::*stop-alert-timers][stop-alert-timers:1]]
(defun chronometrist-third-stop-alert-timers ()
  "Stop timers in `chronometrist-third-timer-list'."
  (mapc (lambda (timer) (cancel-timer timer)) chronometrist-third-timer-list))
;; stop-alert-timers:1 ends here

;; [[file:chronometrist-third.org::*clock-in][clock-in:1]]
(defun chronometrist-third-clock-in (&optional _arg)
  "Stop alert timers and update break time."
  (chronometrist-third-stop-alert-timers)
  (unless (zerop chronometrist-third-break-time)
    (-let* (((&plist :stop stop) (cl-second (chronometrist-to-list (chronometrist-active-backend))))
            (used-break-duration (ts-diff (ts-now) (chronometrist-iso-to-ts stop)))
            (new-break-time      (- chronometrist-third-break-time used-break-duration)))
      (setq chronometrist-third-break-time
            (if (> new-break-time 0)
                new-break-time
              0))))
  (alert "%s left on your break" (format-seconds chronometrist-third-duration-format chronometrist-third-break-time)))
;; clock-in:1 ends here

;; [[file:chronometrist-third.org::*clock-out][clock-out:1]]
(defun chronometrist-third-clock-out (&optional _arg)
  "Update break time based on the latest work interval.
Run `chronometrist-third-alert-functions' to alert user when
break time is up."
  (let* ((latest-work-duration (chronometrist-interval (chronometrist-latest-record (chronometrist-active-backend))))
         (break-time-increment (/ latest-work-duration chronometrist-third-divisor)))
    (cl-incf chronometrist-third-break-time break-time-increment)
    (alert "%s added to break time (%s total)"
           (format-seconds chronometrist-third-duration-format break-time-increment)
           (format-seconds chronometrist-third-duration-format chronometrist-third-break-time))
    ;; start alert timer(s)
    (chronometrist-third-start-alert-timers)))
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

(provide 'chronometrist-third)

;;; chronometrist-third.el ends here
