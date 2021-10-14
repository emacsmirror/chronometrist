;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;; for some reason, setting `nameless-current-name' to "chronometrist"
;; makes all aliases not take effect - probably specific to Org
;; literate programs
((nil . ((nameless-aliases . (("c" . "chronometrist")
                              ("cb" . "chronometrist-backend")
                              ("cc" . "chronometrist-common")
                              ("cd" . "chronometrist-details")
                              ("ce" . "chronometrist-events")
                              ("ck" . "chronometrist-key-values")
                              ("cm" . "chronometrist-migrate")
                              ("cp" . "chronometrist-plist-pp")
                              ("cr" . "chronometrist-report")
                              ("cs" . "chronometrist-statistics")
                              ("cx" . "chronometrist-sexp")))
         (nameless-current-name . "chronometrist")))
 (org-mode
  . ((org-html-self-link-headlines . t)
     (eval . (org-indent-mode))
     (org-html-head
      . (concat "<link rel=\"stylesheet\" "
                "type=\"text/css\" "
                "href=\"../org-doom-molokai.css\" />")))))
