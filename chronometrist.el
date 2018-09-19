(require 'chronometrist-lib)
(require 'chronometrist-report)

;; 2018-08-27T12:45:03+0530

;; BUGS
;; 1. (goto-char (point-max)) -> RET -> the time spent on the last
;;    project in the list will be the first new project suggestion.
;; 2. Start a project before midnight -> after midnight,
;;    chronometrist will display it as active, but the time spent will
;;    be '-' (zero)
;; 3. Create (and start) a _new_ project -> kill buffer -> run
;;    chronometrist -> cursor is not at the new project
;;    - can't reproduce it?
;; 4. Idle timer stops running after some time?

;; Style issues
;; 1. Uses Scheme-style ? and x->y naming conventions instead of
;;    Elisp/CL-style "-p" and "x-to-y"
;;    - ido uses ? for 'completion help', so you can't type ? unless
;;      you unset that o\
;; 2. Should use *earmuffs* for global variables for clarity
;; 3. Should names of major modes (chronometrist-mode,
;;    chronometrist-report-mode) end with -major-mode ?

;; Limitations of timeclock.el
;; 1. Concurrent tasks not permitted
;; 2. timeclock-project-list contains only the projects found in the
;;    timeclock-file - no way for a user to specify tasks beforehand.
;; 3. Uses non-standard slashes in the date instead of dashes (e.g.
;;    "2018/01/01" instead of "2018-01-01") and a space for the
;;    date-time separator instead of T

;; Limitations of tabulated-list-mode
;; 1. Can't mix tabulated and non-tabulated data!!! What if I want
;;    some buttons, separate from the data but part of the same
;;    buffer?!
;;    - adding non-tabular data after calling `tabulated-list-print' -
;;      as we do - works, but is hacky and doesn't always print (e.g.
;;      it vanishes when you sort). Then, you have to ensure you call
;;      it after each time you call `tabulated-list-print' :\
;;    - a post-print hook could help
;;    - maybe use advice?
;; 2. Can't have multi-line headers

;; TODO - use variables instead of hardcoded numbers to determine spacing
;; TODO - remove repetitive calls to (format "%04d-%02d-%02d" (elt seq a) (elt seq b) (elt seq c))

;; ## VARIABLES ##
(defvar chronometrist-buffer-name "*Chronometrist*")
(defvar chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current
line in the `chronometrist' buffer.")

;; ## IDLE TIMER ##
(defun chronometrist-idle-timer ()
  (when (and (chronometrist-buffer-exists? chronometrist-buffer-name)
             (chronometrist-buffer-visible? chronometrist-buffer-name))
    ;; (message "chronometrist-idle-timer run at %s" (format-time-string "%T"))
    (with-current-buffer chronometrist-buffer-name
      (let ((position (point)))
        (tabulated-list-print t)
        (chronometrist-print-non-tabular)
        (goto-char position)))))

;; ## FUNCTIONS ##
(defun chronometrist-current-project ()
  "Return the name of the currently clocked-in project, or nil if
 the user is not clocked in."
  (if (not (timeclock-currently-in-p))
      nil
    (with-current-buffer (find-file-noselect timeclock-file)
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (re-search-forward (concat chronometrist-time-re-file " ") nil t)
        (buffer-substring-no-properties (point) (point-at-eol))))))

(defun chronometrist-project-active? (project)
  "Return t if PROJECT is currently clocked in, else nil."
  (equal (chronometrist-current-project) project))

(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES
SECONDS]. SECONDS must be a positive integer."
  (setq seconds (truncate seconds))
  (let* ((s (% seconds 60))
         (m (% (/ seconds 60) 60))
         (h (/ seconds 3600)))
    (vector h m s)))

(defun chronometrist-entries ()
  "Create entries to be displayed in the buffer created by
`chronometrist'."
  (timeclock-reread-log)
  (->> timeclock-project-list
       (-sort #'string-lessp)
       (--map-indexed (list it
                            (vector (number-to-string (1+ it-index))
                                    it
                                    (-> (chronometrist-project-time-one-day it)
                                        (chronometrist-format-time))
                                    (if (chronometrist-project-active? it)
                                        "*" ""))))))

(defun chronometrist-project-at-point ()
  "Get the project at point in the `chronometrist' buffer."
  (save-excursion
    (beginning-of-line)
    (--> (buffer-substring-no-properties
          (re-search-forward "[0-9]+ +")
          (progn
            (re-search-forward chronometrist-time-re-ui nil t)
            (match-beginning 0)))
         (replace-regexp-in-string "[ \t]*$" "" it))))

(defun chronometrist-goto-last-project ()
  (goto-char (point-min))
  (re-search-forward timeclock-last-project nil t)
  (beginning-of-line))

(defun chronometrist-time-add (a b)
  "Add two vectors in the form [HOURS MINUTES SECONDS] and
return a vector in the same form."
  (let ((h1 (elt a 0))
        (m1 (elt a 1))
        (s1 (elt a 2))
        (h2 (elt b 0))
        (m2 (elt b 1))
        (s2 (elt b 2)))
    (chronometrist-seconds-to-hms (+ (* h1 3600) (* h2 3600)
                        (* m1 60) (* m2 60)
                        s1 s2))))

(defun chronometrist-total-time-one-day (&optional date)
  "Return the total time clocked on DATE (if non-nil) or
 today, as a vector in the form [HOURS MINUTES SECONDS].

DATE must be calendrical information calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (->> timeclock-project-list
       (--map (chronometrist-project-time-one-day it date))
       (-reduce #'chronometrist-time-add)))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (-->
     (chronometrist-total-time-one-day)
     (chronometrist-format-time it)
     (format "\n    %- 26s%s" "Total" it)
     (concat it
             "\n\n    RET - clock in/out"
             "\n    <numeric argument N> RET - clock in/out from <N>th project"
             "\n    r - see weekly report"
             "\n    l - open log file")
     (insert it))))

;; ## MAJOR-MODE ##
(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("#" 3 t)
                               ("Project" 25 t)
                               ("Time" 10 t)
                               ("Active" 3 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'chronometrist-idle-timer)
  (define-key chronometrist-mode-map (kbd "RET") 'chronometrist-toggle-project)
  (define-key chronometrist-mode-map (kbd "l") 'chronometrist-open-timeclock-file)
  (define-key chronometrist-mode-map (kbd "r") 'chronometrist-report))

;; ## COMMANDS ##

(defun chronometrist-toggle-project (&optional arg)
  "In a `chronometrist' buffer, start or stop the project at point."
  (interactive "P")
  (let ((target-project (progn
                          (when arg
                            (goto-char (point-min))
                            (re-search-forward (format "^%d" arg) nil t))
                          (chronometrist-project-at-point)))
        (current-project  (chronometrist-current-project)))
    ;; We change this function so it suggests the project at point
    (cl-letf (((symbol-function 'timeclock-ask-for-project)
               (lambda ()
                 (timeclock-completing-read
                  (format "Clock into which project (default %s): "
                          target-project)
                  (mapcar 'list timeclock-project-list)
                  target-project))))
      ;; If we're clocked in to anything - clock out or change projects
      (if current-project
          (if (equal target-project current-project)
              (timeclock-out nil nil t)
            ;; We don't use timeclock-change because it doesn't prompt for the reason
            (progn
              (timeclock-out nil nil t)
              (timeclock-in nil nil t)))
        ;; Otherwise, run timeclock-in with project at point as default
        ;; suggestion
        (timeclock-in nil nil t)))
    (timeclock-reread-log) ;; required when we create a new activity
    ;; Trying to update partially doesn't update the activity indicator. Why?
    (tabulated-list-print t nil)
    (chronometrist-print-non-tabular)
    (chronometrist-goto-last-project)))

(defun chronometrist (&optional arg)
  "Displays a list of the user's timeclock.el projects and the
time spent on each today, based on their timelog file
`timeclock-file'. The user can hit RET to start/stop projects.
This is the 'listing command' for chronometrist-mode."
  (interactive "P")
  (if arg
      (chronometrist-report)
    (let ((buffer (get-buffer-create chronometrist-buffer-name)))
      (if (chronometrist-buffer-visible? chronometrist-buffer-name)
          (kill-buffer chronometrist-buffer-name)
        (with-current-buffer buffer
          (chronometrist-mode)
          (tabulated-list-print)

          (when chronometrist-hide-cursor
            (make-local-variable 'cursor-type)
            (setq cursor-type nil)
            (hl-line-mode))
          (switch-to-buffer buffer)
          (chronometrist-print-non-tabular)
          (chronometrist-goto-last-project))))))

(provide 'chronometrist)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End: