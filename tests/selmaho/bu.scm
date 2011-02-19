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

(define (bu)
    ; test bu handling for all gismu
    ;
    (define (gismu-bu gismu)
      (define (gismu=? valsi)
        (string=? gismu valsi))

      (let ((gismu-bu (string-append gismu " bu")))
        (mapti 
          `(text
             (paragraphs
               (paragraph
                 (term (sumti (BY-clause (gismu ,(? gismu=? _))
                                         (cmavo (BU "bu"))))))))
          gismu-bu)))

    ; test bu handling for all cmavo
    ; 
    (define (cmavo-bu selmaho cmavo)
      (define (selmaho=? valsi)
        (string=? selmaho (symbol->string valsi)))

      (define (cmavo=? valsi)
        (string=? cmavo valsi))

      (let ((cmavo-bu (string-append cmavo " bu")))
        (case (string->symbol selmaho)

          ; Errata:
          ;
          ; ba'e bu is the ba'e letteral.  Should it be?  Or should
          ; you be able to emphasize the bu-ness of a letteral?
          ;

          ; FAhO termnates the text before SI erases it.
          ;
          ((FAhO)
            (mapti
              '(text (FAhO-clause (cmavo (FAhO "fa'o"))))
              cmavo-bu))

          ; Errata:
          ;
          ; {lo'u bu} and {le'u bu} are both grammatical.
          ; this needs a lot more testing to see if it makes
          ; sense.
          ;

          ; SA swallows back to the beginning of text, since
          ; there is no previous bu.
          ;
          ((SA)
             '())
;            (mapti
;              '(text (sa-clause (SA-clause (cmavo (SA "sa"))))
;                                (BU-clause (cmavo (BU "bu"))))
;              cmavo-bu))

          ; si is permitted at the beginning of text, but is a
          ; no-op.  This leaves bu by itself, which isn't
          ; grammatical. 
          ;
          ((SI)
            (mapti
              '(text (SI-clause (cmavo (SI "si"))))
              cmavo-bu
              "bu"))

          ; su is permitted at the beginning of text, but is a
          ; no-op.  This leaves bu by itself, which isn't
          ; grammatical.
          ;
;          ((SU)
;            (mapti
;              '(text (su-clause (SU-clause (cmavo (SU "su")))))
;              cmavo-bu
;              "bu"))

          ; Y (Errata: Magic Words and the morphology are mutually
          ;    inconsistent on the selma'o of .y.bu) 
          ;
          ((Y)
            (mapti
              '(text
                 (paragraphs
                   (paragraph
                     (term
                       (sumti
                         (BY-clause (cmavo (BY "ybu"))))))))
              cmavo-bu))

          ; Errata:
          ;
          ; {zei bu} is grammatical.  Should it be?
          ;

          ; ZO quotes BU.
          ;
          ((ZO)
            (mapti
              '(text
                 (paragraphs
                   (paragraph
                     (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                             (cmavo (BU "bu"))))))))
              cmavo-bu))

          ; Errata:
          ;
          ; {zoi bu} is grammatical.  Should it be?
          ;

          ; everything else is a BY.
          ;
          (else
            (mapti 
              `(text (paragraphs
                       (paragraph
                         (term
                           (sumti
                             (BY-clause (cmavo ,((? selmaho=? _)
                                                 (? cmavo=? _)))
                                        (cmavo (BU "bu"))))))))
              cmavo-bu)))))


  ; BU is not permitted at the beginning of a text.
  ; effect.
  ;
  (narmapti "bu")

  (let ((rodacmavo (cmavo:gen-select-list)))
    (map-apply cmavo-bu (rodacmavo)))

  (let ((rodagismu (gismu:gen-select-list)))
    (map (compose gismu-bu car) (rodagismu)))

  0)

(test-group "selma'o BU"
  (bu))
