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

(define (si)
    ; test si handling for all gismu
    ;
    (define (gismu-si gismu)
      (define (gismu=? valsi)
        (string=? gismu valsi))

      (let ((gismu-si-gismu (string-append gismu " si " gismu)))
        (mapti 
          `(text (si-clause (gismu ,(? gismu=? _))
                            (SI-clause (cmavo (SI "si"))))
                 (paragraphs
                   (paragraph
                     (sentence
                       (selbri
                         (BRIVLA-clause
                           (gismu ,(? gismu=? _))))))))
          gismu-si-gismu)))

    ; test si handling for all cmavo
    ; 
    (define (cmavo-si selmaho cmavo)
      (define (selmaho=? valsi)
        (string=? selmaho (symbol->string valsi)))

      (define (cmavo=? valsi)
        (string=? cmavo valsi))

      (let ((cmavo-si (string-append cmavo " si")))
        (case (string->symbol selmaho)
          ; FAhO termnates the text before SI erases it.
          ;
          ((FAhO)
            (mapti
              '(text (FAhO-clause (cmavo (FAhO "fa'o"))))
              cmavo-si))

          ; SA swallows back to the beginning of text before
          ; SI can erase it.
          ;
          ((SA)
            (mapti
              '(text (si-clause (SA-clause (cmavo (SA "sa")))
                                (SI-clause (cmavo (SI "si")))))
              cmavo-si))

          ; Each of these clauses run individually, both acting
          ; as if they are at the beginning of the text.
          ;
          ((SI)
            (mapti
              '(text (SI-clause (cmavo (SI "si")))
                     (SI-clause (cmavo (SI "si"))))
              cmavo-si))

          ; Each of these clauses run individually, both acting
          ; as if they are at the beginning of the text.
          ;
          ((SU)
            (mapti
              '(text (su-clause (SU-clause (cmavo (SU "su"))))
                     (SI-clause (cmavo (SI "si"))))
              cmavo-si))

          ; ZO quotes SI.
          ;
          ((ZO)
            (mapti
              '(text
                 (paragraphs
                   (paragraph
                     (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                             (cmavo (SI "si"))))))))
              cmavo-si))

          ; everything else is erased by SI.
          ;
          (else
            (mapti 
              `(text (si-clause (cmavo ,((? selmaho=? _) (? cmavo=? _)))
                                (SI-clause (cmavo (SI "si")))))
              cmavo-si)))))


  ; SI at the beginning of the text is not in error, but has no
  ; effect.
  ;
  (mapti
    '(text (SI-clause (cmavo (SI "si"))))
    "si")
  (mapti
    '(text (SI-clause (cmavo (SI "si")))
           (SI-clause (cmavo (SI "si"))))
    "si si")
  (mapti
    '(text (SI-clause (cmavo (SI "si")))
           (SI-clause (cmavo (SI "si")))
           (SI-clause (cmavo (SI "si"))))
    "si si si")


  ; bu by itself is not grammatical, but si can erase it.
  ;
  (mapti
    '(text (si-clause (cmavo (BU "bu"))
                      (SI-clause (cmavo (SI "si")))))
    "bu si")

;  ; broda bu is a single word.
;  ;
;  (mapti
;    '(text (si-clause (cmavo (BU "bu"))
;                      (SI-clause (cmavo (SI "si")))))
;    "broda bu si")

;  ; LOhU ... LEhU is a single word for purposes of SI erasure.
;  ;
;  (mapti
;    '(text (si-clause
;             (any-string (cmavo (LOhU "lo'u"))
;                         (cmavo (LEhU "le'u")))
;                    (SI-clause (cmavo (SI "si")))))
;
;    "lo'u le'u si")


  ; {zei si} is an ungrammatical use of zei, but it is still
  ; treated as a single word, just like {bu si}.
  ;
  (mapti
    '(text (si-clause (cmavo (ZEI "zei"))
                      (SI-clause (cmavo (SI "si")))))
    "zei si")

  ; zei is a single word for purposes of si.
  ;
  (mapti
    '(text (si-clause (brivla (gismu "broda")
                              (ZEI-clause (cmavo (ZEI "zei")))
                              (gismu "brode"))
                      (SI-clause (cmavo (SI "si")))))
    "broda zei brode si")

  (mapti
    '(text (si-clause (brivla (gismu "broda")
                              (ZEI-clause (cmavo (ZEI "zei")))
                              (gismu "brode")
                              (ZEI-clause (cmavo (ZEI "zei")))
                              (gismu "brodi"))
                      (SI-clause (cmavo (SI "si")))))
    "broda zei brode zei brodi si")

  (mapti
    '(text (si-clause (brivla (gismu "broda")
                              (ZEI-clause (cmavo (ZEI "zei")))
                              (gismu "brode")
                              (ZEI-clause (cmavo (ZEI "zei")))
                              (gismu "brodi")
                              (ZEI-clause (cmavo (ZEI "zei")))
                              (gismu "brodo"))
                      (SI-clause (cmavo (SI "si")))))
    "broda zei brode zei brodi zei brodo si")


  ; {zo si} is the word {si}, but {zo si si} erases it.
  ;
  (mapti
    '(text
       (paragraphs
         (paragraph
           (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                   (cmavo (SI "si"))))))))
    "zo si")

  (mapti
    '(text (si-clause
             (any-word (cmavo (ZO "zo")) (cmavo (SI "si")))
             (SI-clause (cmavo (SI "si")))))
    "zo si si")


  (let ((rodacmavo (cmavo:gen-select-list)))
    (map-apply cmavo-si (rodacmavo)))

  (let ((rodagismu (gismu:gen-select-list)))
    (map (compose gismu-si car) (rodagismu)))

  0)

(test-group "selma'o SI"
  (si))
