(defvar chronometrist-test-file-path-stem
  (format "%stest" (file-name-directory (or load-file-name default-directory))))

(defvar chronometrist-test-backend
  (make-instance 'chronometrist-plist-backend :path chronometrist-test-file-path-stem))

(defvar chronometrist-test-first-record
  '(:name  "Programming"
    :start "2018-01-01T00:00:00+0530"
    :stop  "2018-01-01T01:00:00+0530"))

(defvar chronometrist-test-latest-record
  '(:name  "Programming"
    :tags  (reading)
    :book  "Smalltalk-80: The Language and Its Implementation"
    :start "2020-05-10T16:33:17+0530"
    :stop  "2020-05-10T17:10:48+0530"))

(ert-deftest chronometrist-count-records ()
  (should (= (chronometrist-count-records chronometrist-test-backend) 12)))

(ert-deftest chronometrist-latest-record ()
  (should (equal (chronometrist-latest-record chronometrist-test-backend)
                 chronometrist-test-latest-record)))

(ert-deftest chronometrist-current-task ()
  (chronometrist-insert chronometrist-test-backend '(:name "Test"))
  (should (equal (chronometrist-current-task chronometrist-test-backend) "Test"))
  (chronometrist-sexp-in-file (chronometrist-backend-file chronometrist-test-backend)
    (goto-char (point-max))
    (backward-list)
    (chronometrist-sexp-delete-list)
    (save-buffer))
  (should (not (chronometrist-current-task chronometrist-test-backend))))

(ert-deftest chronometrist-loop-records ()
  ;; `chronometrist-loop-records' is deliberately run twice here
  (should (equal (first (last (chronometrist-loop-records for record in chronometrist-test-backend
                                collect record)))
                 chronometrist-test-first-record))
  (should (equal (first (chronometrist-loop-records for record in chronometrist-test-backend
                          collect record))
                 chronometrist-test-latest-record)))

(ert-deftest chronometrist-list-tasks ()
  (let ((task-list (chronometrist-list-tasks chronometrist-test-backend)))
    (should (listp task-list))
    (should (seq-every-p #'stringp task-list))))
