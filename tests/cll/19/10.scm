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

(define (cll.19.10)
  ; 10.1
  ;
  (mapti
    ('text
      (_ *** (sentence
               (term (sumti (ZO-clause (cmavo (ZO "zo")) (cmavo (SI "si")))))
               (CU-clause (cmavo (CU "cu")))
               (selbri
                 (BRIVLA-clause (gismu "lojbo"))
                 (BRIVLA-clause (gismu "valsi"))))))
    "zo si cu lojbo valsi")

  ; 10.2
  ;
  (mapti ('text (_ *** '(sentence
                          (term
                            (sumti (ZOI-clause (cmavo (ZOI "zoi"))
                                               (cmavo (BY "gy"))
                                               (non-lojban-word "John")
                                               (cmene "is")
                                               (cmavo (A "a"))
                                               (cmene "man")
                                               (cmavo (BY "gy")))))
                          (CU-clause (cmavo (CU "cu")))
                          (selbri
                            (BRIVLA-clause (gismu "glico"))
                            (BRIVLA-clause (gismu "jufra"))))))
         "zoi gy. John is a man .gy. cu glico jufra")

  ; 10.3 (Errata: ungrammatical according to CLL)
  ;
  (mapti
    ('text 
      (_ *** (sentence
               (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
               (selbri (BRIVLA-clause (gismu "djuno")))
               (term (FA-clause (cmavo (FA "fi")))
                     (sumti (LE-clause
                              (cmavo (LE "le"))
                              (selbri (BRIVLA-clause (gismu "valsi")))
                              (GOI-clause
                                (cmavo (GOI "po'u"))
                                (term (sumti (ZOI-clause
                                               (cmavo (ZOI "zoi"))
                                               (cmavo (BY "gy"))
                                               (cmene "gyrations")
                                               (cmavo (BY "gy"))))))))))))
    "mi djuno fi le valsi po'u zoi gy. gyrations .gy.")

  ; 10.4
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                (selbri (BRIVLA-clause (gismu "djuno")))
                (term (FA-clause (cmavo (FA "fi")))
                      (sumti (LE-clause
                               (cmavo (LE "le"))
                               (selbri (BRIVLA-clause (gismu "valsi")))
                               (GOI-clause
                                 (cmavo (GOI "po'u"))
                                 (term (sumti (ZOI-clause
                                                (cmavo (ZOI "zoi"))
                                                (cmavo (JAI "jai"))
                                                (cmene "gyrations")
                                                (cmavo (JAI "jai"))))))))))))
    "mi djuno fi le valsi po'u zoi jai. gyrations .jai")

  ; 10.5
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (ZOI-clause
                               (cmavo (ZOI "zoi"))
                               (cmavo (BY "ry"))
                               (non-lojban-word "sku")
                               (cmavo (BY "ry")))))
                (CU-clause (cmavo (CU "cu")))
                (selbri (BRIVLA-clause (gismu "rafsi")))
                (term
                  (sumti
                    (ZO-clause (cmavo (ZO "zo")) (gismu "cusku")))))))
    "zoi ry. sku .ry. cu rafsi zo cusku")

  ; {lo'u ... le'u} and {zoi} interaction.  lo'u wins.
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (LOhU-clause
                               (cmavo (LOhU "lo'u"))
                               (cmavo (ZOI "zoi"))
                               (gismu "broda")
                               (LEhU-clause (cmavo (LEhU "le'u"))))))
                (selbri (BRIVLA-clause (gismu "broda")))
                (term (sumti (LOhU-clause
                               (cmavo (LOhU "lo'u"))
                               (LEhU-clause (cmavo (LEhU "le'u")))))))))
    "lo'u zoi broda le'u broda lo'u le'u")


  ; 10.6
  ;
  (mapti
    ('text 
      (_ *** '(sentence
                (term (sumti (ZO-clause (cmavo (ZO "zo")) (cmene "bab"))))
                (selbri (BRIVLA-clause (gismu "cmene")))
                (term (sumti (LA-clause
                               (cmavo (LA "la"))
                               (CMENE-clause (cmene "bab"))))))))
    "zo .bab. cmene la bab.")


  ; 10.7
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (ZO-clause (cmavo (ZO "zo"))
                                        (cmene "bab"))))
                (selbri (BRIVLA-clause (gismu "cmene")))
                (term (sumti (LAhE-clause
                               (cmavo (LAhE "la'e"))
                               (sumti (ZO-clause (cmavo (ZO "zo"))
                                                 (cmene "bab")))))))))
    "zo .bab. cmene la'e zo .bab.")


  ; 10.8
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (LAhE-clause
                               (cmavo (LAhE "lu'e"))
                               (sumti (LA-clause
                                        (cmavo (LA "la"))
                                        (CMENE-clause (cmene "bab")))))))
                (selbri (BRIVLA-clause (gismu "cmene")))
                (term (sumti (LA-clause
                               (cmavo (LA "la"))
                               (CMENE-clause (cmene "bab"))))))))
    "lu'e la bab. cmene la bab.")


  ; 10.9
  ;
  (mapti
    ('text
      (_ *** (sentence
               (term (sumti (LA-clause
                              (cmavo (LA "la"))
                              (CMENE-clause (cmene "bab")))))
               (selbri (BRIVLA-clause (gismu "cmene")))
               (term (sumti (LA-clause
                              (cmavo (LA "la"))
                              (CMENE-clause (cmene "bab"))))))))
    "la bab. cmene la bab.")

  ; 10.10
  ;
  (mapti
    ('text
      (_ *** '(sentence
                (term (sumti (ZOI-clause
                               (cmavo (ZOI "la'o"))
                               (cmavo (BY "dy"))
                               (non-lojban-word "Goethe")
                               (cmavo (BY "dy")))))
                (CU-clause (cmavo (CU "cu")))
                (selbri
                  (ME-clause
                   (cmavo (ME "me"))
                   (sumti (ZOI-clause
                            (cmavo (ZOI "la'o"))
                            (cmavo (BY "ly"))
                            (non-lojban-word "Homo")
                            (cmene "sapiens")
                            (cmavo (BY "ly")))))))))
    "la'o dy. Goethe .dy. cu me la'o ly. Homo sapiens .ly.")
  0)


(test-group "CLL 19.10"
  (cll.19.10))
