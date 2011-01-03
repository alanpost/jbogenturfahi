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
  (define (secuxna-selmaho rodaselmaho)
    (match rodaselmaho
           ((_ 'BU)              'BY)

           (('FEhE selmaho)       selmaho)
           (('FEhE 'PA selmaho)   selmaho)

           (('I selmaho)          selmaho)
           (('I selmaho 'NAI)     selmaho)
           (('I 'NA selmaho)      selmaho)

           (('JAI 'PU)            'SE)
           (('JAI 'BAI)           'SE)
           (('JAI 'VA)            'SE)

           (('LAhE selmaho)       selmaho)

           (('LE 'GOhA)           'KOhA)
           (('LE 'SE 'GOhA)       'KOhA)

           (('MOhI selmaho)       selmaho)

            ; negation uses the second selma'o
            ;
           (('NA selmaho)         selmaho)

           (('NAhE selmaho)       selmaho)

            ; quantity prefix uses the second selma'o
            ;
           (('PA selmaho)         selmaho)
           (('PA 'PA selmaho)     selmaho)
           (('PA 'PA 'PA selmaho) selmaho)

            ; the only PU that usess the second selma'o
            ;
           (('PU 'ZAhO)           'ZAhO)

            ; conversion prefix uses the second selma'o
            ;
           ;(('SE selmaho . _)     selmaho)
           (('SE selmaho)         selmaho)
           (('SE selmaho 'NAI)    selmaho)
           (('SE selmaho 'KOhA)   selmaho)

           ; comment out, shouldn't be needed any longer.
           (()                    '())
           ((rodaselmaho . _)     rodaselmaho)))


         ; convert the selmaho from the database to a symbol,
         ; to match the parser.
         ;
  (let* ((selmaho-db  (string->symbol selmaho))

         ; parse this cmavo
         ;
         (gerna       (jbogenturfahi-rafske cmavo))

         ; retrieve the first selma'o from the parsed results 
         ;
         (selmaho-gen (secuxna-selmaho (map car gerna)))

         ; reconstruct the cmavo as it is returned from the parser.
         ;
         (vowel       (char-set #\a #\e #\i #\o #\u))
         (valsi       (reduce (lambda (cmavo valsi)
                                ; concatenate the cmavo returned
                                ; from the parser, but make sure
                                ; we insert mandatory pauses.
                                ;
                                (if (char-set-contains?
                                       vowel
                                       (string-ref cmavo 0))
                                    (string-append valsi "." cmavo)
                                    (string-append valsi     cmavo)))
                              ""
                              (map cadr gerna))))

    ; ignore BU while I'm testing only the morphology.  BU is
    ; handled in the grammar.
    ;
    (if (not (or (string=? "slakabu" cmavo)
                 (string=? "denpabu" cmavo)))
        (begin (test #t (eq?      selmaho-db selmaho-gen))
               (test #t (string=? cmavo valsi))))))

(define (cmavo)
  (let ((rodacmavo (cmavo:gen-select-list)))
    (map-apply test-selmaho (rodacmavo)))
  0)

(test-group "cmavo"
  (cmavo))
