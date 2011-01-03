;;;;
;;;; jbogenturfahi - lo lojbo ke pe'a jajgau ratcu ke'e genturfa'i
;;;;               `-> A Lojban grammar parser
;;;;
;;;; Copyright (c) 2010 ".alyn.post." <alyn.post@lodockikumazvati.org>
;;;;
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;;

(define (sql:gen-stmt db sql process)
        ; compile |sql|.
  (let ((stmt (sqlite3#prepare db sql)))
          ; finalize |stmt|.  Calling this implies that
          ; this invocation of |sql:gen-stmt| is no longer
          ; needed.
    (let ((cleanup (lambda ()
                     (sqlite3#finalize! stmt)
                     #f))
          (query   (lambda a
                     (sqlite3#reset! stmt)
                     (for-each (curry sqlite3#bind! stmt)
                               (seq 0 (length a))
                               a)
                     (process db stmt))))
      (values query cleanup))))

(define (sql:create-table db sql)
  (let ((table (lambda (db stmt)
                 (sqlite3#step! stmt))))
    (call-with-values
      (lambda ()
        (sql:gen-stmt db sql table))
      (lambda (create cleanup)
        ; XXX could be begin0
        (let ((r (create))) (cleanup) r)))))

(define (sql:drop-table db sql)
  (let ((table (lambda (db stmt)
                 (sqlite3#step! stmt))))
    (call-with-values
      (lambda ()
        (sql:gen-stmt db sql table))
      (lambda (create cleanup)
        ; XXX could be begin0
        (let ((r (create))) (cleanup) r)))))

(define (sql:gen-select-id db sql)
  (let ((select (lambda (db stmt)
                  (and (sqlite3#step! stmt)
                       (sqlite3#column-data stmt 0)))))
    (sql:gen-stmt db sql select)))

(define (sql:gen-select-list db sql)
  (let ((select (lambda (db stmt)
                  (let ((count (sqlite3#column-count stmt)))
                    (until (curry sqlite3#step! stmt)
                           (lambda ()
                               (map (curry sqlite3#column-data stmt)
                                    (seq 0 count))))))))
    (sql:gen-stmt db sql select)))

;; sql:gen-insert returns a procedure that
;; when called, updates the database with
;; the supplied arguments.
;;
;; prepare |sql| and return a procedure
;; that binds its arguments to values in
;; |sql|.  run |sql| against |db| and
;; return the last row id after running
;; the query.
;;
(define (sql:gen-insert db sql)
  (let ((insert (lambda (db stmt)
                  (and (sqlite3#step! stmt)
                       (sqlite3#last-insert-rowid db)))))
    (sql:gen-stmt db sql insert)))

(define (sql:gen-delete db sql)
  (let ((delete (lambda (db stmt)
                  (sqlite3#step! stmt))))
    (sql:gen-stmt db sql delete)))

(define (sql:open path)
  (sqlite3#open-database path))

(define (sql:cleanup db)
  (sqlite3#finalize! db))
