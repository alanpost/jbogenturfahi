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

(define (ldcmz-pamoi-pagbu)
  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                 (selbri (GOhA-clause (cmavo (GOhA "mo"))))))))
    ".i ta mo")

  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ti")))))
                 (selbri (GOhA-clause (cmavo (GOhA "mo"))))))))
    ".i ti mo")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                 (selbri (BRIVLA-clause (gismu "rokci")))))))
    ".i ta rokci")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ti")))))
                 (selbri
                   (BRIVLA-clause
                     (gismu "rokci")
                     (UI-clause (cmavo (UI "xu")))))))))
    ".i ti rokci xu")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                 (selbri
                   (NA-clause
                     (cmavo (NA "ja'a"))
                     (selbri (BRIVLA-clause (gismu "rokci")))))))))
    ".i ta ja'a rokci")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                 (selbri
                   (BRIVLA-clause (gismu "ralte")
																	(UI-clause (cmavo (UI "xu")))))
                 (term
									 (sumti (KOhA-clause (cmavo (KOhA "ti")))
                          (NOI-clause
                            (cmavo (NOI "poi"))
                            (sentence
                              (selbri (BRIVLA-clause (gismu "rokci")))))))))))
    ".i mi ralte xu ti poi rokci")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "do")))))
                 (selbri
                   (NA-clause
                     (cmavo (NA "ja'a"))
                     (selbri (BRIVLA-clause (gismu "ralte")))))
                 (term
									 (sumti (KOhA-clause (cmavo (KOhA "ta")))
                          (NOI-clause
                            (cmavo (NOI "poi"))
                            (sentence
                              (selbri (BRIVLA-clause (gismu "rokci")))))))))))
    ".i do ja'a ralte ta poi rokci")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                 (selbri (GOhA-clause (cmavo (GOhA "mo"))))))))
    ".i ta mo")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ti")))))
                 (selbri (BRIVLA-clause (gismu "grana")))))))
    ".i ti grana")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                 (selbri
                   (BRIVLA-clause
                     (gismu "rokci")
                     (UI-clause (cmavo (UI "xu")))))))))
    ".i ta rokci xu")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ti")))))
                 (selbri
                   (NA-clause
                     (cmavo (NA "na"))
                     (selbri (BRIVLA-clause (gismu "rokci"))))))
               (I-clause
                 (cmavo (I "i"))
                 (sentence
                    (term (sumti (KOhA-clause (cmavo (KOhA "ti")))))
                    (selbri (BRIVLA-clause (gismu "grana"))))))))
    ".i ti na rokci .i ti grana")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                 (selbri
                   (BRIVLA-clause (gismu "grana")
																	(UI-clause (cmavo (UI "xu")))))))))
    ".i ta grana xu")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "ti")))))
                 (selbri
                   (NA-clause
                     (cmavo (NA "ja'a"))
                     (selbri (BRIVLA-clause (gismu "grana")))))))))
    ".i ti ja'a grana")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                 (selbri
                   (BRIVLA-clause (gismu "ralte")
																	(UI-clause (cmavo (UI "xu")))))
                 (term
									 (sumti (KOhA-clause (cmavo (KOhA "ta")))
                          (NOI-clause
                            (cmavo (NOI "poi"))
                            (sentence
                              (selbri (BRIVLA-clause (gismu "grana")))))))))))
    ".i mi ralte xu ta poi grana")


  (mapti
    '(text (I-clause (cmavo (I "i")))
           (paragraphs
             (paragraph
               (sentence
                 (term (sumti (KOhA-clause (cmavo (KOhA "do")))))
                 (selbri
                   (NA-clause
                     (cmavo (NA "na"))
                     (selbri (BRIVLA-clause (gismu "ralte")))))
                 (term
									 (sumti (KOhA-clause (cmavo (KOhA "ti")))
                          (NOI-clause
                            (cmavo (NOI "poi"))
                            (sentence
                              (selbri (BRIVLA-clause (gismu "grana"))))))))
               (I-clause
                 (cmavo (I "i"))
                 (sentence
                   (term (sumti (KOhA-clause (cmavo (KOhA "mi")))))
                   (selbri (BRIVLA-clause (gismu "ralte")))
                   (term
                     (sumti
											 (KOhA-clause (cmavo (KOhA "ti")))
                       (NOI-clause
                         (cmavo (NOI "poi"))
                         (sentence
                           (selbri (BRIVLA-clause (gismu "grana"))))))))))))
    ".i do na ralte ti poi grana .i mi ralte ti poi grana")

  0)

(test-group "lo do ckiku ma zvati"
  (ldcmz-pamoi-pagbu))
