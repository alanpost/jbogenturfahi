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
    (text (I-clause
            (cmavo (I "i"))
            (paragraphs
              (paragraph
                (sentence
                  (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                  (selbri (GOhA-clause (cmavo (GOhA "mo")))))))))
    ".i ta mo")

  (mapti
    (text (I-clause
            (cmavo (I "i"))
            (paragraphs
              (paragraph
                (sentence
                  (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                  (selbri (BRIVLA-clause (gismu "rokci"))))))))
    ".i ta rokci")

  (mapti
    (text (I-clause
            (cmavo (I "i"))
            (paragraphs
              (paragraph
                (sentence
                  (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                  (selbri
                    (BRIVLA-clause
                      (gismu "rokci")
                      (UI-clause (cmavo (UI "xu"))))))))))
    ".i ta rokci xu")

  (mapti
    (text (I-clause
            (cmavo (I "i"))
            (paragraphs
              (paragraph
                (sentence
                  (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                  (selbri
                    (NAhE-clause
                      (cmavo (NAhE "je'a"))
                      (BRIVLA-clause (gismu "rokci")))))))))
    ".i ta je'a rokci")

  (mapti
    (text (I-clause
            (cmavo (I "i"))
            (paragraphs
              (paragraph
                (sentence
                  (term (sumti (KOhA-clause (cmavo (KOhA "ta")))))
                  (selbri
                    (NA-clause
                      (cmavo (NA "na"))
                      (selbri (BRIVLA-clause (gismu "grana"))))))))))
    ".i ta na grana")
  0)

(test-group "lo do ckiku ma zvati"
  (ldcmz-pamoi-pagbu))
