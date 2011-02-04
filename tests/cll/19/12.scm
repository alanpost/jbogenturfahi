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

(define (cll.19.12)
  ; 12.1
  ;
  (mapti
    '(text
       (DOI-clause (cmavo (DOI "doi")) (CMENE-clause cmene "lisas"))
       (paragraphs
         (paragraph
           (sentence
             (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
             (selbri (BRIVLA-clause (gismu "djica")))
             (term
               (sumti (LE-clause
                        (cmavo (LE "le"))
                        (selbri
                          (NU-clause
                            (cmavo (NU "nu"))
                            (TO-clause
                              (cmavo (TO "to"))
                              (text (DOI-clause
                                      (cmavo (DOI "doi"))
                                      (CMENE-clause cmene "frank"))
                                    (paragraphs
                                      (paragraph
                                        (sentence
                                          (term
                                            (sumti
                                              (KOhA-clause
                                                (cmavo (KOhA "ko")))))
                                          (selbri
                                            (BRIVLA-clause
                                              (gismu "sisti")))))))
                              (TOI-clause (cmavo (TOI "toi"))))
                            (sentence
                              (term (sumti (KOhA-clause (cmavo (KOhA "do")))))
                              (selbri (BRIVLA-clause (gismu "viska")))
                              (term
                                (sumti (LE-clause
                                         (cmavo (LE "le"))
                                         (selbri
                                           (BRIVLA-clause
                                             (gismu "mlatu"))))))))))))))))
    "doi lisas. mi djica le nu to doi frank. ko sisti toi do viska le mlatu")

  ; 12.2
  ;
  (mapti ('text (_ *** '(sentence
    (term (sumti (LA-clause
                   (cmavo (LA "la"))
                   (CMENE-clause cmene "frank"))))
    (selbri (BRIVLA-clause (gismu "cusku")))
    (term
      (sumti
        (LU-clause
          (cmavo (LU "lu"))
          (text
            (paragraphs
              (paragraph
                (sentence
                  (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                  (selbri (BRIVLA-clause (gismu "prami")))
                  (term
                    (sumti
                      (KOhA-clause
                        (cmavo (KOhA "do"))
                        (TO-clause
                          (cmavo (TO "to'i"))
                          (UI-clause (cmavo (UI "sa'a")))
                          (text
                            (paragraphs
                              (paragraph
                                (sentence
                                  (term
                                    (sumti
                                      (KOhA-clause
                                        (cmavo (KOhA "do")))))
                                  (selbri
                                    (GOhA-clause (cmavo (GOhA "du"))))
                                  (term
                                    (sumti
                                      (LA-clause
                                        (cmavo (LA "la"))
                                        (CMENE-clause cmene
                                                      "djein"))))))))
                          (TOI-clause (cmavo (TOI "toi")))))))))))
          (LIhU-clause (cmavo (LIhU "li'u")))))))))
     "la frank. cusku lu mi prami do to'isa'a do du la djein. toi li'u")

  ; 12.3
  ;
  (mapti
    ('text (_ *** '(sentence
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "frank"))))
      (selbri
        (BRIVLA-clause
          (gismu "prami")
          (SEI-clause
            (cmavo (SEI "sei"))
            (term (sumti (LA-clause
                           (cmavo (LA "la"))
                           (CMENE-clause cmene "frank"))))
            (selbri (BRIVLA-clause (gismu "gleki"))))))
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "djein")))))))
    "la frank. prami sei la frank. gleki la djein.")

  ; 12.4
  ;
  (mapti
    ('text (_ *** '(sentence
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "frank"))))
      (selbri
        (BRIVLA-clause
          (gismu "prami")
          (SEI-clause
            (cmavo (SEI "sei"))
            (selbri (BRIVLA-clause (gismu "gleki"))))))
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "djein")))))))
    "la frank. prami sei gleki la djein.")

  ; 12.5
  ;
  (mapti
    ('text (_ *** '(sentence
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "frank"))))
      (selbri
        (BRIVLA-clause
          (gismu "prami")
          (SEI-clause
            (cmavo (SEI "sei"))
            (selbri
              (BRIVLA-clause
                (gismu "gleki")
                (BE-clause
                  (cmavo (BE "be"))
                  (term (FA-clause (cmavo (FA "fa")))
                        (sumti (LA-clause
                                 (cmavo (LA "la"))
                                 (CMENE-clause cmene "suzn"))))))))))
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "djein")))))))
    "la frank. prami sei gleki be fa la suzn. la djein.")

  ; 12.6
  ;
  (mapti
    ('text (_ *** `(sentence
      (term (sumti (LA-clause
                     (cmavo (LA "la"))
                     (CMENE-clause cmene "djan"))))
      (selbri (BRIVLA-clause (gismu "cusku")))
      (term (sumti (LU-clause
                     (cmavo (LU "lu"))
                     (text (paragraphs (paragraph (sentence
                       (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                       (selbri (BRIVLA-clause (gismu "klama")))
                       (term
                         (sumti (LE-clause
                                  (cmavo (LE "le"))
                                  (selbri
                                    (BRIVLA-clause (gismu "zarci"))))))))))
                     (LIhU-clause (cmavo (LIhU "li'u")))))))))
    "la djan. cusku lu mi klama le zarci li'u")

  ; 12.7
  ;
  (mapti
    '(text (paragraphs (paragraph
       (term
         (sumti
           (LU-clause
             (cmavo (LU "lu"))
             (text (paragraphs (paragraph (sentence
               (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
               (selbri (BRIVLA-clause (gismu "klama")))
               (term
                 (sumti
                   (LE-clause
                     (cmavo (LE "le"))
                     (selbri
                       (BRIVLA-clause
                         (gismu "zarci")
                         (SEI-clause
                           (cmavo (SEI "sei"))
                           (UI-clause (cmavo (UI "sa'a")))
                           (term (sumti (LA-clause
                                          (cmavo (LA "la"))
                                          (CMENE-clause cmene "djan"))))
                           (selbri
                             (BRIVLA-clause
                               (gismu "cusku")
                               (BE-clause
                                 (cmavo (BE "be"))
                                 (term
                                   (sumti
                                     (KOhA-clause
                                       (cmavo (KOhA "dei"))))))))))))))))))
             (LIhU-clause (cmavo (LIhU "li'u")))))))))
    "lu mi klama le zarci seisa'a la djan. cusku be dei li'u")

  ; 12.8
  ;
  (mapti
    '(text (paragraphs (paragraph
       (term
         (sumti (LU-clause
                  (cmavo (LU "lu"))
                  (text (SEI-clause
                          (cmavo (SEI "sei"))
                          (UI-clause (cmavo (UI "sa'a")))
                          (term (sumti (LA-clause
                                         (cmavo (LA "la"))
                                         (CMENE-clause cmene "djan"))))
                          (selbri
                            (BRIVLA-clause
                              (gismu "cusku")
                              (BE-clause
                                (cmavo (BE "be"))
                                (term
                                  (sumti
                                    (KOhA-clause (cmavo (KOhA "dei")))))))))
                        (paragraphs
                          (paragraph
                            (sentence
                              (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                              (selbri (BRIVLA-clause (gismu "klama")))
                              (term
                                (sumti (LE-clause
                                         (cmavo (LE "le"))
                                         (selbri
                                           (BRIVLA-clause
                                             (gismu "zarci"))))))))))))))))
    "lu seisa'a la djan. cusku be dei mi klama le zarci")


  ; 12.9
  ;
  (mapti
    '(text (paragraphs (paragraph
       (term
         (sumti
           (LU-clause
             (cmavo (LU "lu"))
             (text (paragraphs
                     (paragraph
                       (sentence
                         (term
                           (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                         (selbri
                           (BRIVLA-clause
                             (gismu "klama")
                             (SEI-clause
                               (cmavo (SEI "sei"))
                               (UI-clause (cmavo (UI "sa'a")))
                               (term (sumti (LA-clause
                                              (cmavo (LA "la"))
                                              (CMENE-clause cmene "djan"))))
                               (selbri (BRIVLA-clause (gismu "cusku"))))))
                         (term
                           (sumti (LE-clause
                                    (cmavo (LE "le"))
                                    (selbri
                                      (BRIVLA-clause
                                        (gismu "zarci"))))))))))))))))
    "lu mi klama seisa'a la djan cusku le zarci")

  0)


(test-group "CLL 19.12"
  (cll.19.12))
