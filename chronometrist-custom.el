;;; chronometrist-custom.el --- Custom definitions for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;;; Commentary:
;;

;;; Code:

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

(declare-function chronometrist-format-time "chronometrist-common")
(declare-function chronometrist-last-expr "chronometrist-sexp")
(eval-when-compile (require 'subr-x))

(defcustom chronometrist-activity-indicator "*"
  "How to indicate that a task is active.
Can be a string to be displayed, or a function which returns this string.
The default is \"*\""
  :type '(choice string function))

(defcustom chronometrist-day-start-time "00:00:00"
  "The time at which a day is considered to start, in \"HH:MM:SS\".

The default is midnight, i.e. \"00:00:00\"."
  :type 'string)

(defcustom chronometrist-time-targets-list nil
  "List to specify daily time goals for each project.

Each element must be in the form (TARGET PROJECT *).

TARGET is an integer specifying number of minutes.

PROJECT is the project on which you would like spend TARGET time.

There can be more than one PROJECT, to specify that you would
like to spend TARGET time on any one of those projects.")

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist-custom)

;;; chronometrist-custom.el ends here
