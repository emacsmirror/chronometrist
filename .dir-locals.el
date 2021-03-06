;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;; for some reason, setting `nameless-current-name' to "chronometrist"
;; makes all aliases not take effect - probably specific to Org
;; literate programs
((nil . ((nameless-aliases . (("c" . "chronometrist")
                              ("cc" . "chronometrist-common")
                              ("cd" . "chronometrist-details")
                              ("ce" . "chronometrist-events")
                              ("ck" . "chronometrist-key-values")
                              ("cm" . "chronometrist-migrate")
                              ("cp" . "chronometrist-plist-pp")
                              ("cr" . "chronometrist-report")
                              ("cs" . "chronometrist-statistics")
                              ("cx" . "chronometrist-sexp")))))
 (org-mode
  . ((org-html-self-link-headlines . t)
     (eval . (org-indent-mode))
     (org-html-head
      . (concat "<link rel=\"stylesheet\" "
                "type=\"text/css\" "
                "href=\"../org-doom-molokai.css\" />"))
     (eval
      . (add-hook
         'after-save-hook
         (lambda ()
           (let ((fn (buffer-file-name)))
             (when (y-or-n-p (format "Tangle file %s?" fn))
               (compile
                (mapconcat #'shell-quote-argument
                           `("emacs" "-q" "-Q" "--batch" "--eval=(require 'ob-tangle)"
                             ,(format "--eval=(org-babel-tangle-file \"%s\")" fn))
                           " ")))))
         nil t))
     (eval . (add-hook 'before-save-hook (lambda nil (org-align-all-tags)) nil t)))))
