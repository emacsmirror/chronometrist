;;; chronometrist-spark.el --- Show sparklines in Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://tildegit.org/contrapunctus/chronometrist
;; Package-Requires: ((chronometrist "0.7.0") (literate-elisp "0.1") (spark "0.1"))
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
;; This package adds a column to Chronometrist displaying sparklines for each task.

;;; Code:
(require 'literate-elisp)

(literate-elisp-load
 (format "%schronometrist-spark.org" (file-name-directory load-file-name)))

(provide 'chronometrist-spark)
;;; chronometrist-spark.el ends here
