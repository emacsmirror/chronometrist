(defvar chronometrist-test-file-path-stem
  (format "%stest" (file-name-directory (or load-file-name default-directory))))

(defvar chronometrist-test-backend
  (make-instance 'chronometrist-plist-backend
                 :path chronometrist-test-file-path-stem))

(ert-deftest chronometrist-count-records ()
  (should (= (chronometrist-count-records chronometrist-test-backend) 12)))

(ert-deftest chronometrist-latest-record ()
  (let ((latest-record
         '(:name  "Programming"
           :tags  (reading)
           :book  "Smalltalk-80: The Language and Its Implementation"
           :start "2020-05-10T16:33:17+0530"
           :stop  "2020-05-10T17:10:48+0530")))
    (should (equal (chronometrist-latest-record chronometrist-test-backend)
                   chronometrist-latest-record))))

(ert-deftest chronometrist-current-task ()
  (chronometrist-insert chronometrist-test-backend '(:name "Test"))
  (should (equal (chronometrist-current-task chronometrist-test-backend) "Test"))
  (chronometrist-sexp-in-file (chronometrist-backend-file chronometrist-test-backend)
    (goto-char (point-max))
    (backward-list)
    (chronometrist-sexp-delete-list)
    (save-buffer))
  (should (not (chronometrist-current-task chronometrist-test-backend))))

(ert-deftest chronometrist-task-list ()
  (let ((task-list (chronometrist-list-tasks chronometrist-test-backend)))
    (should (listp task-list))
    (should (seq-every-p #'stringp task-list))))

# Local Variables:
# nameless-aliases: (("c" . "chronometrist"))
# nameless-current-name: "chronometrist"
# End:
