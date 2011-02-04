;;;;
;;;; jbogenturfahi - lo lojbo ke pe'a jajgau ratcu ke'e genturfa'i
;;;;               `-> A Lojban grammar parser
;;;;
;;;; Copyright (c) 2011 ".alyn.post." <alyn.post@lodockikumazvati.org>
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

(define (cll.19.13)
  ; 13.1
  ;
  (mapti
    ('text
      (_ *** (sentence
               (term (sumti (KOhA-clause
                              (cmavo (KOhA "ti"))
                              (si-clause
                                (gismu "gerku")
                                (SI-clause (cmavo (SI "si")))))))
               (selbri (BRIVLA-clause (gismu "mlatu"))))))
    "ti gerku si mlatu")

  ; 13.2
  ;
  (mapti
    ('text
      (_ *** (sentence
               (term (sumti (KOhA-clause
                              (cmavo (KOhA "ta"))
                              (si-clause
                                (gismu "blanu")
                                (si-clause (gismu "zdani")
                                           (SI-clause (cmavo (SI "si"))))
                                (SI-clause (cmavo (SI "si")))))))
               (selbri
                 (BRIVLA-clause (gismu "xekri"))
                 (BRIVLA-clause (gismu "zdani"))))))
    "ta blanu zdani si si xekri zdani")

  ; 13.3 (Errata: one fewer si required because of ((Magic Words))
  ;      rule #1.
  ;
  (mapti
    ('text
      (_ *** (sentence
               (term (sumti (ZO-clause (cmavo (ZO "zo")) (cmene "bab"))))
               (selbri
                 (SE-clause
                   (cmavo (SE "se"))
                   (BRIVLA-clause
                     (gismu "cmene")
                     (si-clause
                       (any-word (cmavo (ZO "zo")) (cmavo (SI "si")))
                       (SI-clause (cmavo (SI "si")))))))
               (term (sumti (LA-clause
                              (cmavo (LA "la"))
                              (CMENE-clause cmene "bab")))))))
    "zo .bab. se cmene zo si si la bab.")

  ; 13.4
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                (selbri (BRIVLA-clause (gismu "tavla")))
                (term (FA-clause (cmavo (FA "fo")))
                      (sumti (LA-clause
                               (cmavo (LA "la"))
                               (si-clause
                                 (cmavo (A "e"))
                                 (si-clause
                                   (lujvo "speranto")
                                   (SI-clause (cmavo (SI "si"))))
                                 (SI-clause (cmavo (SI "si"))))
                               (CMENE-clause cmene "esperanton")))))))
    "mi tavla fo la .esperanto si si .esperanton.")

  ; 13.5 (Errata: only one si is required to erase a zoi quotation
  ;      according to ((Magic Words)).
  ;
  (mapti
    ('text
       (_ *** '(sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                 (selbri
                   (BRIVLA-clause
                     (gismu "cusku")
                     (si-clause
                       (any-string
                         (cmavo (ZOI "zoi"))
                         (cmavo (BY "fy"))
                         (cmavo (BY "gy"))
                         (cmavo (BY "fy")))
                       (SI-clause (cmavo (SI "si"))))))
                 (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                         (cmene "djan")))))))
    "mi cusku zoi fy. gy. .fy. si zo .djan")

  ; 13.6 (Errata: only one si is required to erase a zo quotation
  ;      according to ((Magic Words))
  ;
  (mapti
    ('text
       (_ *** '(sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                 (selbri
                   (SE-clause
                     (cmavo (SE "se"))
                     (BRIVLA-clause
                       (gismu "cmene")
                       (si-clause
                         (any-word (cmavo (ZO "zo")) (cmene "djan"))
                         (SI-clause (cmavo (SI "si")))))))
                 (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                         (cmene "djordj")))))))
    "mi se cmene zo .djan. si zo .djordj.")

  ; 13.7
  ;
;  (mapti
;    '(text
;       (_ *** '(sentence
;    "mi viska le sa .i mi cusku zo .djan.")

  ; 13.8
  ;
  (mapti
    ('text
       (_ *** '(sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                 (selbri (BRIVLA-clause (gismu "cusku")))
                 (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                         (cmene "djan")))))))
    "mi cusku zo .djan.")

  ; 13.9
  ;
;  (mapti
;    '(text
;       (_ *** '(sentence
;    "mi viska le blanu zdan. sa le xekri zdani")

  0)


(test-group "CLL 19.13"
  (cll.19.13))
