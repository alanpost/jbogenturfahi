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

;;
;; lerfu
;;
(define (a lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (e lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (i lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (o lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (u lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (y lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (l lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (m lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (n lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (r lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (b lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (d lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (g lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (v lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (j lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (z lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (s lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (c lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (x lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (k lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (f lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (p lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (t lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (h lerfu #!key (comma '()))
  (string-append (apply string comma) (string lerfu)))

(define (digit digit #!key (comma '()))
  (string-append (apply string comma) (string digit)))


;;
;; parts
;;
(define (final-syllable onset nucleus)
  (string-append onset nucleus))

(define (any-syllable onset nucleus #!key (coda ""))
  (string-append onset nucleus coda))

(define (syllable onset nucleus #!key (coda ""))
  (string-append onset nucleus coda))

(define (consonantal-syllable consonant0 syllabic #!key (consonant ""))
  (string-append consonant0 syllabic consonant))

(define (coda #!key (syllabic "") (consonant ""))
  (string-append syllabic consonant))

(define (onset glide #!key consonant)
  (string-append consonant glide))


(define (diphthong vowel0 vowel1)
  (string-append vowel0 vowel1))


(define (cluster cfari fanmo)
  (apply string-append cfari fanmo))

(define (initial-pair consonant0 consonant1)
  (string-append consonant0 consonant1))

(define (initial #!key (sibilant "") (other "") (liquid ""))
  (string-append sibilant other liquid))

(define (affricate stop fricative)
  (string-append stop fricative))


;; cmene
;;
(define (zifcme consonant #!key (nafanmo '()))
  `(cmene ,(string (apply string-append nafanmo) consonant)))

(define (jbocme #!key (any-syllable-or-digit '()))
  `(cmene ,(apply string-append any-syllable-or-digit)))


;; cmavo
;;
(define (CVCy-lujvo-brivla-core CVC-rafsi
                                y
                                brivla-core
                                #!key (h "")
                                      (initial-rafsi '()))
  (string-append CVC-rafsi
                 y
                 h
                 (apply string-append initial-rafsi)
                 brivla-core))

(define (CVCy-lujvo-short-final-rafsi stressed-CVC-rafsi y short-final-rafsi)
  (string-append stressed-CVC-rafsi y short-final-rafsi))

(define (cmavo-form onset nucleus #!key (h '()))
  (string-append onset
                 (apply string-append
                        (map (lambda (h) (apply string-append h)) h))
                 nucleus))

(define (cmavo-form-y y)
  (apply string-append y))


;; lujvo
;;
(define (lujvo brivla-core #!key (initial-rafsi '()))
  `(lujvo ,(string-append (apply string-append initial-rafsi) brivla-core)))


;; fu'ivla
;;
(define (fuhivla fuhivla-head
                 stressed-syllable
                 final-syllable
                 #!key (consonantal-syllable '()))
  `(fuhivla ,(string-append fuhivla-head
                            stressed-syllable
                            (apply string-append consonantal-syllable)
                            final-syllable)))

(define (stressed-brivla-rafsi brivla-head stressed-syllable h y)
  (string-append brivla-head stressed-syllable h y))

(define (brivla-rafsi brivla-head h0 y #!key (h ""))
  (string-append brivla-head h0 y h))

(define (stressed-fuhivla-rafsi fuhivla-head stressed-syllable onset y)
  (string-append fuhivla-head stressed-syllable onset y))

(define (fuhivla-rafsi fuhivla-head onset y #!key (h ""))
  (string-append fuhivla-head onset y h))

(define (brivla-head #!key (unstressed-syllable '()))
  (apply string-append unstressed-syllable))

(define (slinkuhi consonant rafsi-string)
  (string-append consonant rafsi-string))

(define (rafsi-string rest #!key (y-less-rafsi '()))
  (string-append (apply string-append y-less-rafsi) rest))

(define (rafsi-string-short-final stressed-y-less-rafsi short-final-rafsi)
  (string-append stressed-y-less-rafsi short-final-rafsi))

(define (rafsi-string-initial-pair initial-pair
                                    y
                                    #!key (stressed-y-less-rafsi ""))
  (string-append stressed-y-less-rafsi initial-pair y))


;; gismu
;;
(define (gismu initial consonant vowel)
  `(gismu ,(string-append (apply string-append initial) consonant vowel)))

(define (CVV-final-rafsi consonant stressed-vowel h vowel)
  (string-append consonant stressed-vowel h vowel))

(define (short-final-rafsi rafsi)
  (apply string-append rafsi))

(define (stressed-y-rafsi rafsi y)
  (string-append rafsi y))

(define (stressed-long-rafsi-CCVC initial-pair stressed-vowel consonant)
  (string-append initial-pair stressed-vowel consonant))

(define (stressed-long-rafsi-CVCC consonant0
                                  stressed-vowel
                                  consonant1
                                  consonant2)
  (string-append consonant0 stressed-vowel consonant1 consonant2))

(define (stressed-CVC-rafsi consonant0 stressed-vowel consonant1)
  (string-append consonant0 stressed-vowel consonant1))

(define (stressed-CCV-rafsi initial-pair stressed-vowel)
  (string-append initial-pair stressed-vowel))

(define (stressed-CVV-rafsi consonant vowel #!key (r-hyphen ""))
  (string-append consonant vowel r-hyphen))

(define (stressed-CVV-rafsi-VhV unstressed-vowel h stressed-vowel)
  (string-append unstressed-vowel h stressed-vowel))

(define (y-rafsi rafsi y #!key (h ""))
  (string-append rafsi y h))

(define (long-rafsi-CCVC initial-pair unstressed-vowel consonant)
  (string-append initial-pair unstressed-vowel consonant))

(define (long-rafsi-CVCC consonant0 unstressed-vowel consonant1 consonant2)
  (string-append consonant0 unstressed-vowel consonant1 consonant2))

(define (CVC-rafsi consonant0 unstressed-vowel consonant1)
  (string-append consonant0 unstressed-vowel consonant1))

(define (CCV-rafsi initial-pair unstressed-vowel)
  (string-append initial-pair unstressed-vowel))

(define (CVV-rafsi consonant vowel #!key (r-hyphen ""))
  (string-append consonant vowel r-hyphen))

(define (CVV-rafsi-VhV unstressed-vowel0 h unstressed-vowel1)
  (string-append unstressed-vowel0 h unstressed-vowel1))


;; non-Lojban word
;;
(define (non-lojban-word . rodalerfu)
  `(non-lojban-word ,(apply string rodalerfu)))


;; spaces
;;
(define (ybu y bu)
  `(cmavo (BY ,(string-append (cadadr y) (cadadr bu)))))


;;
;; selma'o
;;
(define (cmavo cmavo-form)
  `(cmavo ,cmavo-form))

(define (A . rodalerfu)
  `(cmavo (A ,(apply string-append rodalerfu))))

(define (BAI . rodalerfu)
  `(cmavo (BAI ,(apply string-append rodalerfu))))

(define (BAhE . rodalerfu)
  `(cmavo (BAhE ,(apply string-append rodalerfu))))

(define (BE . rodalerfu)
  `(cmavo (BE ,(apply string-append rodalerfu))))

(define (BEI . rodalerfu)
  `(cmavo (BEI ,(apply string-append rodalerfu))))

(define (BEhO . rodalerfu)
  `(cmavo (BEhO ,(apply string-append rodalerfu))))

(define (BIhE . rodalerfu)
  `(cmavo (BIhE ,(apply string-append rodalerfu))))

(define (BIhI . rodalerfu)
  `(cmavo (BIhI ,(apply string-append rodalerfu))))

(define (BO . rodalerfu)
  `(cmavo (BO ,(apply string-append rodalerfu))))

(define (BOI . rodalerfu)
  `(cmavo (BOI ,(apply string-append rodalerfu))))

(define (BU . rodalerfu)
  `(cmavo (BU ,(apply string-append rodalerfu))))

(define (BY . rodalerfu)
  `(cmavo (BY ,(apply string-append rodalerfu))))

(define (CAhA . rodalerfu)
  `(cmavo (CAhA ,(apply string-append rodalerfu))))

(define (CAI . rodalerfu)
  `(cmavo (CAI ,(apply string-append rodalerfu))))

(define (CEI . rodalerfu)
  `(cmavo (CEI ,(apply string-append rodalerfu))))

(define (CEhE . rodalerfu)
  `(cmavo (CEhE ,(apply string-append rodalerfu))))

(define (CO . rodalerfu)
  `(cmavo (CO ,(apply string-append rodalerfu))))

(define (COI . rodalerfu)
  `(cmavo (COI ,(apply string-append rodalerfu))))

(define (CU . rodalerfu)
  `(cmavo (CU ,(apply string-append rodalerfu))))

(define (CUhE . rodalerfu)
  `(cmavo (CUhE ,(apply string-append rodalerfu))))

(define (DAhO . rodalerfu)
  `(cmavo (DAhO ,(apply string-append rodalerfu))))

(define (DOI . rodalerfu)
  `(cmavo (DOI ,(apply string-append rodalerfu))))

(define (DOhU . rodalerfu)
  `(cmavo (DOhU ,(apply string-append rodalerfu))))

(define (FA . rodalerfu)
  `(cmavo (FA ,(apply string-append rodalerfu))))

(define (FAhA . rodalerfu)
  `(cmavo (FAhA ,(apply string-append rodalerfu))))

(define (FAhO . rodalerfu)
  `(cmavo (FAhO ,(apply string-append rodalerfu))))

(define (FEhE . rodalerfu)
  `(cmavo (FEhE ,(apply string-append rodalerfu))))

(define (FEhU . rodalerfu)
  `(cmavo (FEhU ,(apply string-append rodalerfu))))

(define (FIhO . rodalerfu)
  `(cmavo (FIhO ,(apply string-append rodalerfu))))

(define (FOI . rodalerfu)
  `(cmavo (FOI ,(apply string-append rodalerfu))))

(define (FUhA . rodalerfu)
  `(cmavo (FUhA ,(apply string-append rodalerfu))))

(define (FUhE . rodalerfu)
  `(cmavo (FUhE ,(apply string-append rodalerfu))))

(define (FUhO . rodalerfu)
  `(cmavo (FUhO ,(apply string-append rodalerfu))))

(define (GA . rodalerfu)
  `(cmavo (GA ,(apply string-append rodalerfu))))

(define (GAhO . rodalerfu)
  `(cmavo (GAhO ,(apply string-append rodalerfu))))

(define (GEhU . rodalerfu)
  `(cmavo (GEhU ,(apply string-append rodalerfu))))

(define (GI . rodalerfu)
  `(cmavo (GI ,(apply string-append rodalerfu))))

(define (GIhA . rodalerfu)
  `(cmavo (GIhA ,(apply string-append rodalerfu))))

(define (GOI . rodalerfu)
  `(cmavo (GOI ,(apply string-append rodalerfu))))

(define (GOhA . rodalerfu)
  `(cmavo (GOhA ,(apply string-append rodalerfu))))

(define (GUhA . rodalerfu)
  `(cmavo (GUhA ,(apply string-append rodalerfu))))

(define (I . rodalerfu)
  `(cmavo (I ,(apply string-append rodalerfu))))

(define (JA . rodalerfu)
  `(cmavo (JA ,(apply string-append rodalerfu))))

(define (JAI . rodalerfu)
  `(cmavo (JAI ,(apply string-append rodalerfu))))

(define (JOhI . rodalerfu)
  `(cmavo (JOhI ,(apply string-append rodalerfu))))

(define (JOI . rodalerfu)
  `(cmavo (JOI ,(apply string-append rodalerfu))))

(define (KE . rodalerfu)
  `(cmavo (KE ,(apply string-append rodalerfu))))

(define (KEhE . rodalerfu)
  `(cmavo (KEhE ,(apply string-append rodalerfu))))

(define (KEI . rodalerfu)
  `(cmavo (KEI ,(apply string-append rodalerfu))))

(define (KI . rodalerfu)
  `(cmavo (KI ,(apply string-append rodalerfu))))

(define (KOhA . rodalerfu)
  `(cmavo (KOhA ,(apply string-append rodalerfu))))

(define (KU . rodalerfu)
  `(cmavo (KU ,(apply string-append rodalerfu))))

(define (KUhE . rodalerfu)
  `(cmavo (KUhE ,(apply string-append rodalerfu))))

(define (KUhO . rodalerfu)
  `(cmavo (KUhO ,(apply string-append rodalerfu))))

(define (LA . rodalerfu)
  `(cmavo (LA ,(apply string-append rodalerfu))))

(define (LAU . rodalerfu)
  `(cmavo (LAU ,(apply string-append rodalerfu))))

(define (LAhE . rodalerfu)
  `(cmavo (LAhE ,(apply string-append rodalerfu))))

(define (LE . rodalerfu)
  `(cmavo (LE ,(apply string-append rodalerfu))))

(define (LEhU . rodalerfu)
  `(cmavo (LEhU ,(apply string-append rodalerfu))))

(define (LI . rodalerfu)
  `(cmavo (LI ,(apply string-append rodalerfu))))

(define (LIhU . rodalerfu)
  `(cmavo (LIhU ,(apply string-append rodalerfu))))

(define (LOhO . rodalerfu)
  `(cmavo (LOhO ,(apply string-append rodalerfu))))

(define (LOhU . rodalerfu)
  `(cmavo (LOhU ,(apply string-append rodalerfu))))

(define (LU . rodalerfu)
  `(cmavo (LU ,(apply string-append rodalerfu))))

(define (LUhU . rodalerfu)
  `(cmavo (LUhU ,(apply string-append rodalerfu))))

(define (MAhO . rodalerfu)
  `(cmavo (MAhO ,(apply string-append rodalerfu))))

(define (MAI . rodalerfu)
  `(cmavo (MAI ,(apply string-append rodalerfu))))

(define (ME . rodalerfu)
  `(cmavo (ME ,(apply string-append rodalerfu))))

(define (MEhU . rodalerfu)
  `(cmavo (MEhU ,(apply string-append rodalerfu))))

(define (MOhE . rodalerfu)
  `(cmavo (MOhE ,(apply string-append rodalerfu))))

(define (MOhI . rodalerfu)
  `(cmavo (MOhI ,(apply string-append rodalerfu))))

(define (MOI . rodalerfu)
  `(cmavo (MOI ,(apply string-append rodalerfu))))

(define (NA . rodalerfu)
  `(cmavo (NA ,(apply string-append rodalerfu))))

(define (NAI . rodalerfu)
  `(cmavo (NAI ,(apply string-append rodalerfu))))

(define (NAhE . rodalerfu)
  `(cmavo (NAhE ,(apply string-append rodalerfu))))

(define (NAhU . rodalerfu)
  `(cmavo (NAhU ,(apply string-append rodalerfu))))

(define (NIhE . rodalerfu)
  `(cmavo (NIhE ,(apply string-append rodalerfu))))

(define (NIhO . rodalerfu)
  `(cmavo (NIhO ,(apply string-append rodalerfu))))

(define (NOI . rodalerfu)
  `(cmavo (NOI ,(apply string-append rodalerfu))))

(define (NU . rodalerfu)
  `(cmavo (NU ,(apply string-append rodalerfu))))

(define (NUhA . rodalerfu)
  `(cmavo (NUhA ,(apply string-append rodalerfu))))

(define (NUhI . rodalerfu)
  `(cmavo (NUhI ,(apply string-append rodalerfu))))

(define (NUhU . rodalerfu)
  `(cmavo (NUhU ,(apply string-append rodalerfu))))

(define (PA . rodalerfu)
  `(cmavo (PA ,(apply string-append rodalerfu))))

(define (PEhE . rodalerfu)
  `(cmavo (PEhE ,(apply string-append rodalerfu))))

(define (PEhO . rodalerfu)
  `(cmavo (PEhO ,(apply string-append rodalerfu))))

(define (PU . rodalerfu)
  `(cmavo (PU ,(apply string-append rodalerfu))))

(define (RAhO . rodalerfu)
  `(cmavo (RAhO ,(apply string-append rodalerfu))))

(define (ROI . rodalerfu)
  `(cmavo (ROI ,(apply string-append rodalerfu))))

(define (SA . rodalerfu)
  `(cmavo (SA ,(apply string-append rodalerfu))))

(define (SE . rodalerfu)
  `(cmavo (SE ,(apply string-append rodalerfu))))

(define (SEI . rodalerfu)
  `(cmavo (SEI ,(apply string-append rodalerfu))))

(define (SEhU . rodalerfu)
  `(cmavo (SEhU ,(apply string-append rodalerfu))))

(define (SI . rodalerfu)
  `(cmavo (SI ,(apply string-append rodalerfu))))

(define (SOI . rodalerfu)
  `(cmavo (SOI ,(apply string-append rodalerfu))))

(define (SU . rodalerfu)
  `(cmavo (SU ,(apply string-append rodalerfu))))

(define (TAhE . rodalerfu)
  `(cmavo (TAhE ,(apply string-append rodalerfu))))

(define (TEhU . rodalerfu)
  `(cmavo (TEhU ,(apply string-append rodalerfu))))

(define (TEI . rodalerfu)
  `(cmavo (TEI ,(apply string-append rodalerfu))))

(define (TO . rodalerfu)
  `(cmavo (TO ,(apply string-append rodalerfu))))

(define (TOI . rodalerfu)
  `(cmavo (TOI ,(apply string-append rodalerfu))))

(define (TUhE . rodalerfu)
  `(cmavo (TUhE ,(apply string-append rodalerfu))))

(define (TUhU . rodalerfu)
  `(cmavo (TUhU ,(apply string-append rodalerfu))))

(define (UI . rodalerfu)
  `(cmavo (UI ,(apply string-append rodalerfu))))

(define (VA . rodalerfu)
  `(cmavo (VA ,(apply string-append rodalerfu))))

(define (VAU . rodalerfu)
  `(cmavo (VAU ,(apply string-append rodalerfu))))

(define (VEI . rodalerfu)
  `(cmavo (VEI ,(apply string-append rodalerfu))))

(define (VEhO . rodalerfu)
  `(cmavo (VEhO ,(apply string-append rodalerfu))))

(define (VEhA . rodalerfu)
  `(cmavo (VEhA ,(apply string-append rodalerfu))))

(define (VIhA . rodalerfu)
  `(cmavo (VIhA ,(apply string-append rodalerfu))))

(define (VUhO . rodalerfu)
  `(cmavo (VUhO ,(apply string-append rodalerfu))))

(define (VUhU . rodalerfu)
  `(cmavo (VUhU ,(apply string-append rodalerfu))))

(define (XI . rodalerfu)
  `(cmavo (XI ,(apply string-append rodalerfu))))

(define (Y . rodalerfu)
  `(cmavo (Y ,(apply string-append rodalerfu))))

(define (ZAhO . rodalerfu)
  `(cmavo (ZAhO ,(apply string-append rodalerfu))))

(define (ZEhA . rodalerfu)
  `(cmavo (ZEhA ,(apply string-append rodalerfu))))

(define (ZEI . rodalerfu)
  `(cmavo (ZEI ,(apply string-append rodalerfu))))

(define (ZI . rodalerfu)
  `(cmavo (ZI ,(apply string-append rodalerfu))))

(define (ZIhE . rodalerfu)
  `(cmavo (ZIhE ,(apply string-append rodalerfu))))

(define (ZO . rodalerfu)
  `(cmavo (ZO ,(apply string-append rodalerfu))))

(define (ZOI . rodalerfu)
  `(cmavo (ZOI ,(apply string-append rodalerfu))))

(define (ZOhU . rodalerfu)
  `(cmavo (ZOhU ,(apply string-append rodalerfu))))


