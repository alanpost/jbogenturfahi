(use utf8)
(use utf8-srfi-13)
(use utf8-srfi-14)
(use genturfahi)
(use genturfahi-peg)
(use data-structures)
(use sqlite3)

; override the database path, as we can't copy it to the install
; directory until we've created it.
;
(define jbogenturfahi-db-file (make-parameter "jbogenturfahi.db"))

(include "c0re.scm")
(include "sql.scm")
(include "db.scm")

(include "wordlists/cmavo.scm")
(include "wordlists/gismu.scm")
(include "wordlists/rafsi.scm")
