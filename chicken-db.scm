(use srfi-13)
(use srfi-14)
(use genturfahi)
(use genturfahi-peg)
(use sqlite3)

; override the database path, as we can't copy it to the install
; directory until we've created it.
;
(define jbogenturfahi-db-path (make-parameter "jbogenturfahi.db"))

(include "c0re.scm")
(include "sql.scm")
(include "db.scm")

(include "wordlists/cmavo.scm")
