;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((nameless-aliases . (("cc" . "chronometrist-common")
                                          ("cd" . "chronometrist-details")
                                          ("ce" . "chronometrist-events")
                                          ("ck" . "chronometrist-key-values")
                                          ("cm" . "chronometrist-migrate")
                                          ("cp" . "chronometrist-plist-pp")
                                          ("cr" . "chronometrist-report")
                                          ("cs" . "chronometrist-statistics")
                                          ("cx" . "chronometrist-sexp")
                                          ("c" . "chronometrist")))
                     (outline-regexp . ";;;+ ")))
 (org-mode . ((nameless-aliases . (("cc" . "chronometrist-common")
                                   ("cd" . "chronometrist-details")
                                   ("ce" . "chronometrist-events")
                                   ("ck" . "chronometrist-key-values")
                                   ("cm" . "chronometrist-migrate")
                                   ("cp" . "chronometrist-plist-pp")
                                   ("cr" . "chronometrist-report")
                                   ("cs" . "chronometrist-statistics")
                                   ("cx" . "chronometrist-sexp")
                                   ("c" . "chronometrist")))
              (org-tags-column . -60)
              (org-html-self-link-headlines . t)
              (eval . (org-indent-mode))
              (org-html-head
               . (concat "<link rel=\"stylesheet\" "
                         "type=\"text/css\" "
                         "href=\"../org-doom-molokai.css\" />"))
              (eval . (progn
                        (make-local-variable 'after-save-hook)
                        (add-hook 'after-save-hook
                                (lambda nil
                                  (interactive)
                                  (compile
                                   (mapconcat #'shell-quote-argument
                                              `("emacs" "-q" "-Q" "--batch" "--eval=(require 'ob-tangle)"
                                                ,(format "--eval=(org-babel-tangle-file \"%s\")" (buffer-file-name)))
                                              " ")))
                                nil t))))))
