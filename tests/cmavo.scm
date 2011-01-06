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

(define (test-selmaho selmaho cmavo)
  (define (secuxna-selmaho selmaho-db gerna)
    (match gerna
       ; letteral conversion
      ((('cmavo (selmaho pamoi))
        ('cmavo ('BU     remoi)))
        (test #t (eq? selmaho-db 'BY))
        (test #t (string=? cmavo (string-append pamoi remoi))))


      ((('cmavo ('FEhE   pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('FEhE   pamoi))
        ('cmavo ('PA     remoi))
        ('cmavo (selmaho cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))


       ; handle I prefixed entries.
       ;
      ((('cmavo ('I      pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('I      pamoi))
        ('cmavo (selmaho remoi))
        ('cmavo ('NAI    cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))

      ((('cmavo ('I      pamoi))
        ('cmavo ('NA     remoi))
        ('cmavo (selmaho cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))


       ; Why are these SE in cmavo.txt?
       ;
      ((('cmavo ('JAI pamoi))
        ('cmavo ('VA  remoi)))
        (test #t (eq? selmaho-db 'SE))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('JAI pamoi))
        ('cmavo ('BAI remoi)))
        (test #t (eq? selmaho-db 'SE))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('JAI pamoi))
        ('cmavo ('PU  remoi)))
        (test #t (eq? selmaho-db 'SE))
        (test #t (string=? cmavo (string-append pamoi remoi))))


      ((('cmavo ('LAhE   pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))


       ; I'm not sure why cmavo.txt considers these KOhA.
       ;
      ((('cmavo ('LE   pamoi))
        ('cmavo ('GOhA remoi)))
        (test #t (eq? selmaho-db 'KOhA))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('LE   pamoi))
        ('cmavo ('SE   remoi))
        ('cmavo ('GOhA cimoi)))
        (test #t (eq? selmaho-db 'KOhA))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))


       ; not sure about this case.
       ;
      ((('cmavo ('MOhI   pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))


       ; negation uses the second selma'o
       ;
      ((('cmavo ('NA     pamoi))
        ('cmavo ('A      remoi)))
        (test #t (eq? selmaho-db 'A))
        (test #t (string=? cmavo (string-append pamoi "." remoi))))

      ((('cmavo ('NA     pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('NAhE   pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))


       ; quantity prefix uses the second selma'o
       ;
      ((('cmavo ('PA     pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('PA     pamoi))
        ('cmavo ('PA     remoi))
        ('cmavo (selmaho cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))

      ((('cmavo ('PA     pamoi))
        ('cmavo ('PA     remoi))
        ('cmavo ('PA     cimoi))
        ('cmavo (selmaho vomoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi vomoi))))


       ; the only PU that usess the second selma'o.  The rest
       ; are captured by the default rule.
       ;
      ((('cmavo ('PU   pamoi))
        ('cmavo ('ZAhO remoi)))
        (test #t (eq? selmaho-db 'ZAhO))
        (test #t (string=? cmavo (string-append pamoi remoi))))


      ; conversion prefix uses the second selma'o
      ;
      ((('cmavo ('SE     pamoi))
        ('cmavo (selmaho remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo ('SE     pamoi))
        ('cmavo (selmaho remoi))
        ('cmavo ('KOhA   cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))

      ((('cmavo ('SE     pamoi))
        ('cmavo (selmaho remoi))
        ('cmavo ('NAI    cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))


      ; the typical pattern
      ;
      ((('cmavo (selmaho pamoi))
        ('cmavo (_       remoi))
        ('cmavo (_       cimoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi cimoi))))

      ((('cmavo (selmaho pamoi))
        ('cmavo (_       remoi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo (string-append pamoi remoi))))

      ((('cmavo (selmaho valsi)))
        (test #t (eq? selmaho-db selmaho))
        (test #t (string=? cmavo valsi)))))


  ; ignore BU while I'm testing only the morphology.  BU is
  ; handled in the grammar.
  ;
  (if (not (or (string=? "slakabu" cmavo)
               (string=? "denpabu" cmavo)))
           ; convert the selmaho from the database to a symbol,
           ; to match the parser.
           ;
    (let* ((selmaho-db  (string->symbol selmaho))

           ; parse this cmavo
           ;
           (gerna       (jbogenturfahi-rafske cmavo)))

      (secuxna-selmaho selmaho-db gerna))))

(define (cmavo)
  (let ((rodacmavo (cmavo:gen-select-list)))
    (map-apply test-selmaho (rodacmavo)))
  0)

(test-group "cmavo"
  (cmavo))
