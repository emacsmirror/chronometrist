;;; chronometrist-migrate.el --- Commands to aid in migrating from timeclock to chronometrist s-expr format -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'seq)
(require 'chronometrist-common)
(require 'chronometrist-time)
(require 'chronometrist-plist-pp)

(defvar chronometrist-file)
(defvar chronometrist-migrate-table (make-hash-table))

;; TODO - support other timeclock codes (currently only "i" and "o"
;; are supported.)
(defun chronometrist-migrate-populate (in-file)
  "Read data from IN-FILE to `chronometrist-migrate-table'.

IN-FILE should be a file in the format supported by timeclock.el.
See `timeclock-log-data' for a description."
  (clrhash chronometrist-migrate-table)
  (with-current-buffer (find-file-noselect in-file)
    (save-excursion
      (goto-char (point-min))
      (let ((key-counter 0))
        (while (not (eobp))
          (let* ((event-string (buffer-substring-no-properties (point-at-bol)
                                                               (point-at-eol)))
                 (event-list   (split-string event-string "[ /:]"))
                 (code         (cl-first event-list))
                 (date-time    (--> event-list
                                    (seq-drop it 1)
                                    (seq-take it 6)
                                    (mapcar #'string-to-number it)
                                    (reverse it)
                                    (apply #'encode-time it)
                                    (chronometrist-format-time-iso8601 it)))
                 (project-or-comment
                  (replace-regexp-in-string
                   (rx (and (or "i" "o") " "
                            (and (= 4 digit) "/" (= 2 digit) "/" (= 2 digit) " ")
                            (and (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
                            (opt " ")))
                   ""
                   event-string)))
            (pcase code
              ("i"
               (cl-incf key-counter)
               (puthash key-counter
                        `(:name ,project-or-comment :start ,date-time)
                        chronometrist-migrate-table))
              ("o"
               (--> (gethash key-counter chronometrist-migrate-table)
                    (append it
                            `(:stop ,date-time)
                            (when (and (stringp project-or-comment)
                                       (not
                                        (string= project-or-comment "")))
                              `(:comment ,project-or-comment)))
                    (puthash key-counter it chronometrist-migrate-table)))))
          (forward-line)
          (goto-char (point-at-bol))))
      nil)))

(defvar timeclock-file)

(defun chronometrist-migrate-timelog-file->sexp-file (&optional in-file out-file)
  "Migrate your existing `timeclock-file' to the Chronometrist file format.

IN-FILE and OUT-FILE, if provided, are used as input and output
file names respectively."
  (interactive `(,(if (featurep 'timeclock)
                      (read-file-name (concat "timeclock file (default: "
                                              timeclock-file
                                              "): ")
                                      user-emacs-directory
                                      timeclock-file t)
                    (read-file-name (concat "timeclock file: ")
                                    user-emacs-directory
                                    nil t))
                 ,(read-file-name (concat "Output file (default: "
                                          (locate-user-emacs-file "chronometrist.sexp")
                                          "): ")
                                  user-emacs-directory
                                  (locate-user-emacs-file "chronometrist.sexp"))))
  (when (if (file-exists-p out-file)
            (yes-or-no-p (concat "Output file "
                                 out-file
                                 " already exists - overwrite? "))
          t)
    (let ((output (find-file-noselect out-file)))
      (with-current-buffer output
        (chronometrist-common-clear-buffer output)
        (chronometrist-migrate-populate in-file)
        (maphash (lambda (_key value)
                   (chronometrist-plist-pp value output)
                   (insert "\n\n"))
                 chronometrist-migrate-table)
        (save-buffer)))))

(defun chronometrist-migrate-check ()
  "Offer to import data from `timeclock-file' if `chronometrist-file' does not exist."
  (when (and (bound-and-true-p timeclock-file)
             (not (file-exists-p chronometrist-file)))
    (if (yes-or-no-p (format (concat "Chronometrist v0.3+ uses a new file format;"
                                     " import data from %s ? ")
                             timeclock-file))
        (chronometrist-migrate-timelog-file->sexp-file timeclock-file chronometrist-file)
      (message "You can migrate later using `chronometrist-migrate-timelog-file->sexp-file'."))))

(provide 'chronometrist-migrate)

;;; chronometrist-migrate.el ends here