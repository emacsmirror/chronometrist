;;; chronometrist-key-values.el --- add key-values to Chronometrist data -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabjab.de>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((chronometrist "0.7.0"))
;; Version: 0.1.0

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;
;; This package lets users attach tags and key-values to their tracked time, similar to tags and properties in Org mode.
;;
;; To use, add one or more of these functions to any chronometrist hook except `chronometrist-before-in-functions'.
;; * `chronometrist-tags-add'
;; * `chronometrist-kv-add'
;; * `chronometrist-key-values-unified-prompt'

;;; Code:
(require 'chronometrist)

(defun chronometrist-history-prep (key history-table)
  "Prepare history of KEY in HISTORY-TABLE for use in prompts.
Each value in hash table TABLE must be a list.  Each value will be reversed and will have duplicate elements removed."
  (--> (gethash key history-table)
       (cl-remove-duplicates it :test #'equal :from-end t)
       (puthash key it history-table)))

(defun chronometrist-keyword-to-string (keyword)
  "Return KEYWORD as a string, with the leading \":\" removed."
  (replace-regexp-in-string "^:?" "" (symbol-name keyword)))

(defun chronometrist-maybe-string-to-symbol (list)
  "For each string in LIST, if it has no spaces, convert it to a symbol."
  (cl-loop for string in list
    if (string-match-p "[[:space:]]" string)
    collect string
    else collect (intern string)))

(defun chronometrist-maybe-symbol-to-string (list)
  "Convert each symbol in LIST to a string."
  (--map (if (symbolp it)
             (symbol-name it)
           it)
         list))

(defun chronometrist-plist-update (old-plist new-plist)
  "Add tags and keyword-values from NEW-PLIST to OLD-PLIST.
OLD-PLIST and NEW-PLIST should be a property lists.

Keywords reserved by Chronometrist - :name, :start, and :stop -
will not be updated. Keywords in OLD-PLIST with new values in
NEW-PLIST will be updated. Tags in OLD-PLIST will be preserved
alongside new tags from NEW-PLIST."
  (-let* (((&plist :name  old-name  :tags old-tags
                   :start old-start :stop old-stop) old-plist)
          ;; Anything that's left will be the user's key-values.
          (old-kvs   (chronometrist-plist-key-values old-plist))
          ;; Prevent the user from adding reserved key-values.
          (plist     (chronometrist-plist-key-values new-plist))
          (new-tags  (-> (append old-tags (plist-get new-plist :tags))
                         (cl-remove-duplicates :test #'equal)))
          ;; In case there is an overlap in key-values, we use
          ;; plist-put to replace old ones with new ones.
          (new-kvs   (cl-copy-list old-plist))
          (new-kvs   (if plist
                         (-> (cl-loop for (key val) on plist by #'cddr
                               do (plist-put new-kvs key val)
                               finally return new-kvs)
                             (chronometrist-plist-key-values))
                       old-kvs)))
    (append `(:name ,old-name)
            (when new-tags `(:tags ,new-tags))
            new-kvs
            `(:start ,old-start)
            (when old-stop `(:stop  ,old-stop)))))

(defvar chronometrist-tags-history (make-hash-table :test #'equal)
  "Hash table of tasks and past tag combinations.
Each value is a list of tag combinations, in reverse
chronological order. Each combination is a list containing tags
as symbol and/or strings.")

(defun chronometrist-tags-history-populate (task history-table backend)
  "Store tag history for TASK in HISTORY-TABLE from FILE.
Return the new value inserted into HISTORY-TABLE.

HISTORY-TABLE must be a hash table. (see `chronometrist-tags-history')"
  (puthash task nil history-table)
  (cl-loop for plist in (chronometrist-to-list backend) do
    (let ((new-tag-list  (plist-get plist :tags))
          (old-tag-lists (gethash task history-table)))
      (and (equal task (plist-get plist :name))
           new-tag-list
           (puthash task
                    (if old-tag-lists
                        (append old-tag-lists (list new-tag-list))
                      (list new-tag-list))
                    history-table))))
  (chronometrist-history-prep task history-table))

(defvar chronometrist--tag-suggestions nil
  "Suggestions for tags.
Used as history by `chronometrist-tags-prompt'.")

(defun chronometrist-tags-history-add (plist)
  "Add tags from PLIST to `chronometrist-tags-history'."
  (let* ((table    chronometrist-tags-history)
         (name     (plist-get plist :name))
         (tags     (plist-get plist :tags))
         (old-tags (gethash name table)))
    (when tags
      (--> (cons tags old-tags)
           (puthash name it table)))))

(defun chronometrist-tags-history-combination-strings (task)
  "Return list of past tag combinations for TASK.
Each combination is a string, with tags separated by commas.

This is used to provide history for `completing-read-multiple' in
`chronometrist-tags-prompt'."
  (->> (gethash task chronometrist-tags-history)
       (mapcar (lambda (list)
                 (->> list
                      (mapcar (lambda (elt)
                                (if (stringp elt)
                                    elt
                                  (symbol-name elt))))
                      (-interpose ",")
                      (apply #'concat))))))

(defun chronometrist-tags-history-individual-strings (task)
  "Return list of tags for TASK, with each tag being a single string.
This is used to provide completion for individual tags, in
`completing-read-multiple' in `chronometrist-tags-prompt'."
  (--> (gethash task chronometrist-tags-history)
    (-flatten it)
    (cl-remove-duplicates it :test #'equal)
    (cl-loop for elt in it
      collect (if (stringp elt)
                  elt
                (symbol-name elt)))))

(defun chronometrist-tags-prompt (task &optional initial-input)
  "Read one or more tags from the user and return them as a list of strings.
TASK should be a string.
INITIAL-INPUT is as used in `completing-read'."
  (setq chronometrist--tag-suggestions (chronometrist-tags-history-combination-strings task))
  (completing-read-multiple (concat "Tags for " task " (optional): ")
                            (chronometrist-tags-history-individual-strings task)
                            nil
                            'confirm
                            initial-input
                            'chronometrist--tag-suggestions))

(defun chronometrist-tags-add (&rest _args)
  "Read tags from the user; add them to the last entry in `chronometrist-file'.
_ARGS are ignored. This function always returns t, so it can be
used in `chronometrist-before-out-functions'."
  (interactive)
  (let* ((backend   (chronometrist-active-backend))
         (last-expr (chronometrist-latest-record backend))
         (last-name (plist-get last-expr :name))
         (_history  (chronometrist-tags-history-populate last-name chronometrist-tags-history backend))
         (last-tags (plist-get last-expr :tags))
         (input     (->> (chronometrist-maybe-symbol-to-string last-tags)
                         (-interpose ",")
                         (apply #'concat)
                         (chronometrist-tags-prompt last-name)
                         (chronometrist-maybe-string-to-symbol))))
    (when input
      (--> (append last-tags input)
           (reverse it)
           (cl-remove-duplicates it :test #'equal)
           (reverse it)
           (list :tags it)
           (chronometrist-plist-update
            (chronometrist-latest-record backend) it)
           (chronometrist-replace-last backend it)))
    t))

(defgroup chronometrist-key-values nil
  "Add key-values to Chronometrist time intervals."
  :group 'chronometrist)

(defcustom chronometrist-key-value-use-database-history t
  "If non-nil, use database to generate key-value suggestions.
If nil, only `chronometrist-key-value-preset-alist' is used."
  :type 'boolean
  :group 'chronometrist-key-value)

(defcustom chronometrist-key-value-preset-alist nil
  "Alist of key-value suggestions for `chronometrist-key-value' prompts.
Each element must be in the form (\"TASK\" <KEYWORD> <VALUE> ...)"
  :type
  '(repeat
    (cons
     (string :tag "Task name")
     (repeat :tag "Property preset"
             (plist :tag "Property"
                    ;; :key-type 'keyword :value-type 'sexp
                    ))))
  :group 'chronometrist-key-values)

(defun chronometrist-key-value-get-presets (task)
  "Return presets for TASK from `chronometrist-key-value-preset-alist' as a list of plists."
  (alist-get task chronometrist-key-value-preset-alist nil nil #'equal))

(defcustom chronometrist-kv-buffer-name "*Chronometrist-Key-Values*"
  "Name of buffer in which key-values are entered."
  :group 'chronometrist-key-values
  :type 'string)

(defvar chronometrist-key-history
  (make-hash-table :test #'equal)
  "Hash table to store previously-used user-keys.
Each hash key is the name of a task. Each hash value is a list
containing keywords used with that task, in reverse chronological
order. The keywords are stored as strings and their leading \":\"
is removed.")

(defun chronometrist-key-history-populate (task history-table backend)
  "Store key history for TASK in HISTORY-TABLE from FILE.
Return the new value inserted into HISTORY-TABLE.

HISTORY-TABLE must be a hash table (see `chronometrist-key-history')."
  (puthash task nil history-table)
  (cl-loop for plist in backend do
    (catch 'quit
      (let* ((name     (plist-get plist :name))
             (_check   (unless (equal name task) (throw 'quit nil)))
             (keys     (--> (chronometrist-plist-key-values plist)
                            (seq-filter #'keywordp it)
                            (cl-loop for key in it collect
                              (chronometrist-keyword-to-string key))))
             (_check   (unless keys (throw 'quit nil)))
             (old-keys (gethash name history-table)))
        (puthash name
                 (if old-keys (append old-keys keys) keys)
                 history-table))))
  (chronometrist-history-prep task history-table))

(defvar chronometrist-value-history
  (make-hash-table :test #'equal)
  "Hash table to store previously-used values for user-keys.
The hash table keys are user-key names (as strings), and the
values are lists containing values (as strings).")

(defun chronometrist-value-history-populate (history-table backend)
  "Store value history in HISTORY-TABLE from FILE.
HISTORY-TABLE must be a hash table. (see `chronometrist-value-history')"
  (clrhash history-table)
  ;; Note - while keys are Lisp keywords, values may be any Lisp
  ;; object, including lists
  (cl-loop for plist in (chronometrist-to-list backend) do
    ;; We call them user-key-values because we filter out Chronometrist's
    ;; reserved key-values
    (let ((user-key-values (chronometrist-plist-key-values plist)))
      (cl-loop for (key value) on user-key-values by #'cddr do
        (let* ((key-string (chronometrist-keyword-to-string key))
               (old-values (gethash key-string history-table))
               (value      (if (not (stringp value)) ;; why?
                               (list (format "%S" value))
                             (list value))))
          (puthash key-string
                   (if old-values (append old-values value) value)
                   history-table)))))
  (maphash (lambda (key _values)
             (chronometrist-history-prep key history-table))
           history-table))

(defvar chronometrist--value-suggestions nil
  "Suggestions for values.
Used as history by `chronometrist-value-prompt'.")

(defvar chronometrist-kv-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chronometrist-kv-accept)
    (define-key map (kbd "C-c C-k") #'chronometrist-kv-reject)
    map)
  "Keymap used by `chronometrist-kv-read-mode'.")

(define-derived-mode chronometrist-kv-read-mode emacs-lisp-mode "Key-Values"
  "Mode used by `chronometrist' to read key values from the user."
  (->> ";; Use \\[chronometrist-kv-accept] to accept, or \\[chronometrist-kv-reject] to cancel\n"
       (substitute-command-keys)
       (insert)))

(defun chronometrist-kv-completion-quit-key ()
  "Return appropriate keybinding (as a string) to quit from `completing-read'.
It currently supports ido, ido-ubiquitous, ivy, and helm."
  (substitute-command-keys
   (cond ((or (bound-and-true-p ido-mode)
              (bound-and-true-p ido-ubiquitous-mode))
          "\\<ido-completion-map>\\[ido-select-text]")
         ((bound-and-true-p ivy-mode)
          "\\<ivy-minibuffer-map>\\[ivy-immediate-done]")
         ((bound-and-true-p helm-mode)
          "\\<helm-comp-read-map>\\[helm-cr-empty-string]")
         (t "leave blank"))))

(defun chronometrist-key-prompt (used-keys)
  "Prompt the user to enter keys.
USED-KEYS are keys they have already added since the invocation
of `chronometrist-kv-add'."
  (let ((key-suggestions (--> (chronometrist-latest-record (chronometrist-active-backend))
                           (plist-get it :name)
                           (gethash it chronometrist-key-history))))
    (completing-read (format "Key (%s to quit): "
                             (chronometrist-kv-completion-quit-key))
                     ;; don't suggest keys which have already been used
                     (cl-loop for used-key in used-keys do
                       (setq key-suggestions
                             (seq-remove (lambda (key)
                                           (equal key used-key))
                                         key-suggestions))
                       finally return key-suggestions)
                     nil nil nil 'key-suggestions)))

(defun chronometrist-value-prompt (key)
  "Prompt the user to enter values.
KEY should be a string for the just-entered key."
  (setq chronometrist--value-suggestions (gethash key chronometrist-value-history))
  (completing-read (format "Value (%s to quit): "
                           (chronometrist-kv-completion-quit-key))
                   chronometrist--value-suggestions nil nil nil
                   'chronometrist--value-suggestions))

(defun chronometrist-value-insert (value)
  "Insert VALUE into the key-value entry buffer."
  (insert " ")
  (cond ((or
          ;; list or vector
          (and (string-match-p (rx (and bos (or "(" "\"" "["))) value)
               (string-match-p (rx (and (or ")" "\"" "]") eos)) value))
          ;; int or float
          (string-match-p "^[0-9]*\\.?[0-9]*$" value))
         (insert value))
        (t (insert "\"" value "\"")))
  (insert "\n"))

(defun chronometrist-kv-add (&rest _args)
  "Read key-values from user, adding them to a temporary buffer for review.
In the resulting buffer, users can run `chronometrist-kv-accept'
to add them to the last s-expression in `chronometrist-file', or
`chronometrist-kv-reject' to cancel.

_ARGS are ignored. This function always returns t, so it can be
used in `chronometrist-before-out-functions'."
  (interactive)
  (let* ((buffer      (get-buffer-create chronometrist-kv-buffer-name))
         (first-key-p t)
         (backend     (chronometrist-active-backend))
         (last-sexp   (chronometrist-latest-record backend))
         (last-name   (plist-get last-sexp :name))
         (last-kvs    (chronometrist-plist-key-values last-sexp))
         (used-keys   (--map (chronometrist-keyword-to-string it)
                             (seq-filter #'keywordp last-kvs))))
    (chronometrist-key-history-populate last-name chronometrist-key-history backend)
    (chronometrist-value-history-populate chronometrist-value-history backend)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (chronometrist-kv-read-mode)
      (if (and (chronometrist-current-task (chronometrist-active-backend)) last-kvs)
          (progn
            (funcall chronometrist-sexp-pretty-print-function last-kvs buffer)
            (down-list -1)
            (insert "\n "))
        (insert "()")
        (down-list -1))
      (catch 'empty-input
        (let (input key value)
          (while t
            (setq key (chronometrist-key-prompt used-keys)
                  input key
                  used-keys (append used-keys
                                    (list key)))
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (unless first-key-p
                (insert " "))
              (insert ":" key)
              (setq first-key-p nil))
            (setq value (chronometrist-value-prompt key)
                  input value)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (chronometrist-value-insert value)))))
      (chronometrist-sexp-reindent-buffer))
    t))

(defun chronometrist-kv-accept ()
  "Accept the plist in `chronometrist-kv-buffer-name' and add it to `chronometrist-file'."
  (interactive)
  (let* ((backend (chronometrist-active-backend))
         (latest  (chronometrist-latest-record backend))
         user-kv-expr)
    (with-current-buffer (get-buffer chronometrist-kv-buffer-name)
      (goto-char (point-min))
      (setq user-kv-expr (ignore-errors (read (current-buffer))))
      (kill-buffer chronometrist-kv-buffer-name))
    (if user-kv-expr
        (chronometrist-replace-last backend (chronometrist-plist-update latest user-kv-expr))
      (chronometrist-refresh))))

(defun chronometrist-kv-reject ()
  "Reject the property list in `chronometrist-kv-buffer-name'."
  (interactive)
  (kill-buffer chronometrist-kv-buffer-name)
  (chronometrist-refresh))

(easy-menu-define chronometrist-key-value-menu chronometrist-mode-map
  "Key value menu for Chronometrist mode."
  '("Key-Values"
    ["Change tags for active/last interval" chronometrist-tags-add]
    ["Change key-values for active/last interval" chronometrist-kv-add]
    ["Change tags and key-values for active/last interval"
     chronometrist-key-values-unified-prompt]))

(cl-defun chronometrist-key-values-unified-prompt
    (&optional (task (plist-get (chronometrist-latest-record (chronometrist-active-backend)) :name)))
  "Query user for tags and key-values to be added for TASK.
Return t, to permit use in `chronometrist-before-out-functions'."
  (interactive)
  (let* ((backend (chronometrist-active-backend))
         (presets (--map (format "%S" it)
                         (chronometrist-key-value-get-presets task)))
         (key-values
          (when chronometrist-key-value-use-database-history
            (cl-loop for plist in (chronometrist-to-list backend)
              when (equal (plist-get plist :name) task)
              collect
              (let ((plist (chronometrist-plist-remove plist :name :start :stop)))
                (when plist (format "%S" plist)))
              into key-value-plists
              finally return
              (--> (seq-filter #'identity key-value-plists)
                   (cl-remove-duplicates it :test #'equal :from-end t)))))
         (latest (chronometrist-latest-record backend)))
    (if (and (null presets) (null key-values))
        (progn (chronometrist-tags-add) (chronometrist-kv-add))
      (let* ((candidates (append presets key-values))
             (input      (completing-read
                          (format "Key-values for %s: " task)
                          candidates nil nil nil 'chronometrist-key-values-unified-prompt-history)))
        (chronometrist-replace-last backend
                        (chronometrist-plist-update latest
                                        (read input))))))
  t)

(provide 'chronometrist-key-values)
;;; chronometrist-key-values.el ends here
