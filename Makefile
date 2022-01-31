.phony: all setup tangle compile lint clean

all: clean clean-tangle manual.md setup tangle compile lint

setup:
	emacs --batch --eval="(package-initialize)" \
	--eval="(mapcar #'package-install '(indent-lint package-lint relint))"

manual.md:
	emacs -q -Q --batch --eval="(require 'ox-md)" \
	"manual.org" -f 'org-md-export-to-markdown'

# No -q or -Q without ORG_PATH - if the user has a newer version of
# Org, we want to use it.
tangle:
	cd elisp/ && \
        emacs --batch \
	--eval="(package-initialize)" \
        --eval="(require 'ob-tangle)" \
        --eval='(org-babel-tangle-file "chronometrist.org")' \
        --eval='(org-babel-tangle-file "chronometrist-key-values.org")' \
        --eval='(org-babel-tangle-file "chronometrist-spark.org")' \
        --eval='(org-babel-tangle-file "chronometrist-sqlite.org")' ; \
        cd .. ; \

compile: tangle
	cd elisp/ && \
	emacs -q -Q --batch \
        --eval="(progn (package-initialize) (require 'dash) (require 'ts))" \
        --eval='(byte-compile-file "chronometrist.el")' \
        --eval='(byte-compile-file "chronometrist-key-values.el")' \
        --eval='(byte-compile-file "chronometrist-spark.el")' \
        --eval='(byte-compile-file "chronometrist-sqlite.el")' ; \
        cd ..

lint-check-declare: tangle
	cd elisp/ && \
	emacs -q -Q --batch \
        --eval='(check-declare-file "chronometrist.el")' \
        --eval='(check-declare-file "chronometrist-key-values.el")' \
        --eval='(check-declare-file "chronometrist-spark.el")' \
        --eval='(check-declare-file "chronometrist-sqlite.el")' ; \
        cd ..

lint-checkdoc: tangle
	cd elisp/ && \
	emacs -q -Q --batch \
        --eval='(checkdoc-file "chronometrist.el")' \
        --eval='(checkdoc-file "chronometrist-key-values.el")' \
        --eval='(checkdoc-file "chronometrist-spark.el")' \
        --eval='(checkdoc-file "chronometrist-sqlite.el")' ; \
        cd ..

lint-package-lint: setup tangle
	cd elisp/ && \
	emacs -q -Q --batch \
        --eval="(progn (package-initialize) (require 'dash) (require 'ts))" \
        --eval="(require 'package-lint)" \
        -f 'package-lint-batch-and-exit' chronometrist.el \
        -f 'package-lint-batch-and-exit' chronometrist-key-values.el \
        -f 'package-lint-batch-and-exit' chronometrist-spark.el \
        -f 'package-lint-batch-and-exit' chronometrist-sqlite.el ; \
        cd ..

lint-relint: setup tangle
	cd elisp/ && \
	emacs -q -Q --batch \
	--eval="(package-initialize)" \
	--eval="(require 'relint)" \
        --eval='(relint-file "chronometrist.el")' \
        --eval='(relint-file "chronometrist-key-values.el")' \
        --eval='(relint-file "chronometrist-spark.el")' \
        --eval='(relint-file "chronometrist-sqlite.el")' ; \
        cd ..

lint: lint-check-declare lint-checkdoc lint-package-lint lint-relint

clean-tangle:
	rm elisp/chronometrist.el \
            elisp/chronometrist-key-values.el \
            elisp/chronometrist-spark.el \
            elisp/chronometrist-sqlite.el ;

clean-elc:
	rm elisp/*.elc

clean: clean-elc
