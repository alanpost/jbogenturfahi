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

(use jbogenturfahi matchable test)

;; this test is expected to fail, so the input is
;; also the text that remains after the parse.
;;
(define-syntax narmapti
  (syntax-rules ()
    ((_ jufra)
            ; a list containing the parse tree and
            ; the remaining text.
            ;
     (let* ((jalge (jbogenturfahi* jufra))
            (mapti (match (car jalge) (`(text) #t) (_ #f))))
       (test jufra #t    mapti)
       (test jufra jufra (cadr jalge))))))

;; this test is expected to succeed, so the input
;; is fully consumed, leaving nothing after the
;; parse.
;;
(define-syntax mapti
  (syntax-rules ()
    ((_ gensuha jufra)
            ; a list containing the parse tree and
            ; the remaining text.
            ;
     (let* ((jalge (jbogenturfahi* jufra))
            (mapti (match (car jalge) (gensuha #t) (_ #f))))
       (test jufra #t mapti)
       (test jufra "" (cadr jalge))))))


(include "../c0re.scm")
(include "cmavo.scm")
(include "gismu.scm")
(include "rafsi.scm")

(include "ldcmz.scm")

(include "selmaho/zoi.scm")

(include "cll/19/10.scm")
(include "cll/19/13.scm")

(test-exit)
