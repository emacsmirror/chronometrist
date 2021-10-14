(ert-deftest chronometrist-format-duration-long ()
  (should (equal (chronometrist-format-duration-long 5) ""))
  (should (equal (chronometrist-format-duration-long 65) "1 minute"))
  (should (equal (chronometrist-format-duration-long 125) "2 minutes"))

  (should (equal (chronometrist-format-duration-long 3605) "1 hour"))
  (should (equal (chronometrist-format-duration-long 3660) "1 hour, 1 minute"))
  (should (equal (chronometrist-format-duration-long 3725) "1 hour, 2 minutes"))

  (should (equal (chronometrist-format-duration-long 7200) "2 hours"))
  (should (equal (chronometrist-format-duration-long 7260) "2 hours, 1 minute"))
  (should (equal (chronometrist-format-duration-long 7320) "2 hours, 2 minutes")))
