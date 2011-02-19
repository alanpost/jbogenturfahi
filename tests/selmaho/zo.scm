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

(define (zo)
    ; test zo handling for all gismu
    ;
    (define (zo-gismu gismu)
      (define (gismu=? valsi)
        (string=? gismu valsi))

      (let ((zo-gismu (string-append "zo " gismu)))
        (mapti 
          `(text
             (paragraphs
               (paragraph
                 (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                         (gismu ,(? gismu=? _))))))))
          zo-gismu)))

    ; test zo handling for all cmavo
    ; 
    (define (zo-cmavo selmaho cmavo)
      (define (selmaho=? valsi)
        (string=? selmaho (symbol->string valsi)))

      (define (cmavo=? valsi)
        (string=? cmavo valsi))

      (let ((zo-cmavo (string-append "zo " cmavo)))
        ; zo always binds with the following word.
        ;
        (mapti
          `(text
             (paragraphs
               (paragraph
                 (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                         (cmavo (,(? selmaho=? _)
                                                 ,(? cmavo=? _)))))))))
          zo-cmavo)))

  ; ZO without a word to quote is not grammatical.
  ;
  (narmapti "zo")

  ; BU quotes any-word.  ZO wins ZO BU, however:
  ;
  ; zo broda bu => ((zo broda) bu)
  ; zo bu       => (zo bu)
  ;
  (mapti
    '(text
       (paragraphs
         (paragraph
           (term (sumti (BY-clause (any-word (cmavo (ZO "zo"))
                                             (gismu "broda"))
                                   (cmavo (BU "bu"))))))))
    "zo broda bu")

  ; LOhU ... LEhU
  ;
  ; ZO quotes the next word, so this statement is not a valid
  ; LOhU ... LEhU quote.
  ;
  (mapti
    '(text
       (paragraphs
         (paragraph
           (sentence
             (term (sumti (ZO-clause (cmavo (ZO "zo")) (cmavo (LOhU "lo'u")))))
             (selbri (BRIVLA-clause (gismu "broda")))))))
    "zo lo'u broda le'u"
    "le'u")

;  ; ZEI binds the surrounding words.  Ensure that it grabs our
;  ; pseudo word.
;  ;
;  (mapti
;    '()
;    "zo broda zei zo brode")

  ; ZO can be used as the delimiter in ZOI 
  ;
  (mapti
    '(text
       (paragraphs
         (paragraph
           (term (sumti (ZOI-clause
                          (cmavo (ZOI "zoi"))
                          (cmavo (ZO "zo"))
                          (non-lojban-word "Hello,")
                          (non-lojban-word "World")
                          (cmavo (ZO "zo"))))))))
    "zoi zo Hello, World zo")

  ; ZO does not quote the ZOI pseudo-word.
  ;
  (mapti
    '(text
       (paragraphs
         (paragraph
           (sentence
             (term (sumti (ZO-clause (cmavo (ZO "zo")) (cmavo (ZOI "zoi")))))
             (selbri (BRIVLA-clause (gismu "broda"))
                     (BRIVLA-clause (gismu "broda")))))))
    "zo zoi broda broda")

  (let ((rodacmavo (cmavo:gen-select-list)))
    (map-apply zo-cmavo (rodacmavo)))

  (let ((rodagismu (gismu:gen-select-list)))
    (map (compose zo-gismu car) (rodagismu)))

  0)

(test-group "selma'o ZO"
  (zo))
