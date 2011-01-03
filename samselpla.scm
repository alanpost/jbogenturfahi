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
(define (a comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (e comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (i comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (o comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (u comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (y comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (l comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (m comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (n comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (r comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (b comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (d comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (g comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (v comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (j comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (z comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (s comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (c comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (x comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (k comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (f comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (p comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (t comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (h comma lerfu)
  (string-append (apply string comma) (string lerfu)))

(define (digit comma digit)
  (string-append (apply string comma) (string digit)))

(define (comma lerfu)
  lerfu)


;;
;; parts
;;
(define (final-syllable onset nucleus)
  (string-append onset nucleus))

(define (any-syllable onset nucleus coda)
  (string-append onset nucleus coda))

(define (syllable onset nucleus coda)
  (string-append onset nucleus coda))

(define (consonantal-syllable consonant0 syllabic consonant1)
  (string-append consonant0 syllabic consonant1))

(define (coda syllabic consonant)
  (string-append syllabic consonant))

(define (onset consonant glide)
  (string-append consonant glide))


(define (diphthong vowel0 vowel1)
  (string-append vowel0 vowel1))


(define (cluster cfari fanmo)
  (apply string-append cfari fanmo))

(define (initial-pair consonant0 consonant1)
  (string-append consonant0 consonant1))

(define (initial sibilant other liquid)
  (string-append sibilant other liquid))

(define (affricate stop fricative)
  (string-append stop fricative))


;; cmene
;;
(define (zifcme nafanmo consonant)
  `(cmene ,(string (apply string-append nafanmo) consonant)))

(define (jbocme any-syllable-or-digit)
  `(cmene ,(apply string-append any-syllable-or-digit)))


;; cmavo
;;
(define (CVCy-lujvo-brivla-core CVC-rafsi y h initial-rafsi brivla-core)
  (string-append CVC-rafsi
                 y
                 h
                 (apply string-append initial-rafsi)
                 brivla-core))

(define (CVCy-lujvo-short-final-rafsi stressed-CVC-rafsi y short-final-rafsi)
  (string-append stressed-CVC-rafsi y short-final-rafsi))

(define (cmavo-form onset h nucleus)
  (string-append onset
                 (apply string-append
                        (map (lambda (h) (apply string-append h)) h))
                 nucleus))

(define (cmavo-form-y y)
  (apply string-append y))


;; lujvo
;;
(define (lujvo initial-rafsi brivla-core)
  `(lujvo ,(string-append (apply string-append initial-rafsi) brivla-core)))


;; fu'ivla
;;
(define (fuhivla fuhivla-head
                 stressed-syllable
                 consonantal-syllable
                 final-syllable)
  `(fuhivla ,(string-append fuhivla-head
                            stressed-syllable
                            (apply string-append consonantal-syllable)
                            final-syllable)))

(define (stressed-brivla-rafsi brivla-head stressed-syllable h y)
  (string-append brivla-head stressed-syllable h y))

(define (brivla-rafsi brivla-head h0 y h1)
  (string-append brivla-head h0 y h1))

(define (stressed-fuhivla-rafsi fuhivla-head stressed-syllable onset y )
  (string-append fuhivla-head stressed-syllable onset y))

(define (fuhivla-rafsi fuhivla-head onset y h)
  (string-append fuhivla-head onset y h))

(define (brivla-head . unstressed-syllable)
  (apply string-append unstressed-syllable))

(define (slinkuhi consonant rafsi-string)
  (string-append consonant rafsi-string))

(define (rafsi-string y-less-rafsi rest)
  (string-append (apply string-append y-less-rafsi) rest))

(define (rafsi-string-short-final stressed-y-less-rafsi short-final-rafsi)
  (string-append stressed-y-less-rafsi short-final-rafsi))

(define (rafsi-string-initial-pair stressed-y-less-rafsi initial-pair y)
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

(define (stressed-CVV-rafsi consonant vowel0 vowel1)
  (string-append consonant vowel0 vowel1))

(define (stressed-CVV-rafsi-VhV unstressed-vowel h stressed-vowel)
  (string-append unstressed-vowel h stressed-vowel))

(define (y-rafsi rafsi y h)
  (string-append rafsi y h))

(define (long-rafsi-CCVC initial-pair unstressed-vowel consonant)
  (string-append initial-pair unstressed-vowel consonant))

(define (long-rafsi-CVCC consonant0 unstressed-vowel consonant1 consonant2)
  (string-append consonant0 unstressed-vowel consonant1 consonant2))

(define (CVC-rafsi consonant0 unstressed-vowel consonant1)
  (string-append consonant0 unstressed-vowel consonant1))

(define (CCV-rafsi initial-pair unstressed-vowel)
  (string-append initial-pair unstressed-vowel))

(define (CVV-rafsi consonant vowel0 vowel1)
  (string-append consonant vowel0 vowel1))

(define (CVV-rafsi-VhV unstressed-vowel0 h unstressed-vowel1)
  (string-append unstressed-vowel0 h unstressed-vowel1))


;; non-Lojban word
;;
(define (non-lojban-word . rodalerfu)
  `(non-lojban-word ,(apply string rodalerfu)))


;; spaces
;;
(define (ybu y bu)
  `(BY ,(string-append (cadr y) (cadr bu))))


;;
;; selma'o
;;
(define (A . rodalerfu)
  `(A ,(apply string-append rodalerfu)))

(define (BAI . rodalerfu)
  `(BAI ,(apply string-append rodalerfu)))

(define (BAhE . rodalerfu)
  `(BAhE ,(apply string-append rodalerfu)))

(define (BE . rodalerfu)
  `(BE ,(apply string-append rodalerfu)))

(define (BEI . rodalerfu)
  `(BEI ,(apply string-append rodalerfu)))

(define (BEhO . rodalerfu)
  `(BEhO ,(apply string-append rodalerfu)))

(define (BIhE . rodalerfu)
  `(BIhE ,(apply string-append rodalerfu)))

(define (BIhI . rodalerfu)
  `(BIhI ,(apply string-append rodalerfu)))

(define (BO . rodalerfu)
  `(BO ,(apply string-append rodalerfu)))

(define (BOI . rodalerfu)
  `(BOI ,(apply string-append rodalerfu)))

(define (BU . rodalerfu)
  `(BU ,(apply string-append rodalerfu)))

(define (BY . rodalerfu)
  `(BY ,(apply string-append rodalerfu)))

(define (CAhA . rodalerfu)
  `(CAhA ,(apply string-append rodalerfu)))

(define (CAI . rodalerfu)
  `(CAI ,(apply string-append rodalerfu)))

(define (CEI . rodalerfu)
  `(CEI ,(apply string-append rodalerfu)))

(define (CEhE . rodalerfu)
  `(CEhE ,(apply string-append rodalerfu)))

(define (CO . rodalerfu)
  `(CO ,(apply string-append rodalerfu)))

(define (COI . rodalerfu)
  `(COI ,(apply string-append rodalerfu)))

(define (CU . rodalerfu)
  `(CU ,(apply string-append rodalerfu)))

(define (CUhE . rodalerfu)
  `(CUhE ,(apply string-append rodalerfu)))

(define (DAhO . rodalerfu)
  `(DAhO ,(apply string-append rodalerfu)))

(define (DOI . rodalerfu)
  `(DOI ,(apply string-append rodalerfu)))

(define (DOhU . rodalerfu)
  `(DOhU ,(apply string-append rodalerfu)))

(define (FA . rodalerfu)
  `(FA ,(apply string-append rodalerfu)))

(define (FAhA . rodalerfu)
  `(FAhA ,(apply string-append rodalerfu)))

(define (FAhO . rodalerfu)
  `(FAhO ,(apply string-append rodalerfu)))

(define (FEhE . rodalerfu)
  `(FEhE ,(apply string-append rodalerfu)))

(define (FEhU . rodalerfu)
  `(FEhU ,(apply string-append rodalerfu)))

(define (FIhO . rodalerfu)
  `(FIhO ,(apply string-append rodalerfu)))

(define (FOI . rodalerfu)
  `(FOI ,(apply string-append rodalerfu)))

(define (FUhA . rodalerfu)
  `(FUhA ,(apply string-append rodalerfu)))

(define (FUhE . rodalerfu)
  `(FUhE ,(apply string-append rodalerfu)))

(define (FUhO . rodalerfu)
  `(FUhO ,(apply string-append rodalerfu)))

(define (GA . rodalerfu)
  `(GA ,(apply string-append rodalerfu)))

(define (GAhO . rodalerfu)
  `(GAhO ,(apply string-append rodalerfu)))

(define (GEhU . rodalerfu)
  `(GEhU ,(apply string-append rodalerfu)))

(define (GI . rodalerfu)
  `(GI ,(apply string-append rodalerfu)))

(define (GIhA . rodalerfu)
  `(GIhA ,(apply string-append rodalerfu)))

(define (GOI . rodalerfu)
  `(GOI ,(apply string-append rodalerfu)))

(define (GOhA . rodalerfu)
  `(GOhA ,(apply string-append rodalerfu)))

(define (GUhA . rodalerfu)
  `(GUhA ,(apply string-append rodalerfu)))

(define (I . rodalerfu)
  `(I ,(apply string-append rodalerfu)))

(define (JA . rodalerfu)
  `(JA ,(apply string-append rodalerfu)))

(define (JAI . rodalerfu)
  `(JAI ,(apply string-append rodalerfu)))

(define (JOhI . rodalerfu)
  `(JOhI ,(apply string-append rodalerfu)))

(define (JOI . rodalerfu)
  `(JOI ,(apply string-append rodalerfu)))

(define (KE . rodalerfu)
  `(KE ,(apply string-append rodalerfu)))

(define (KEhE . rodalerfu)
  `(KEhE ,(apply string-append rodalerfu)))

(define (KEI . rodalerfu)
  `(KEI ,(apply string-append rodalerfu)))

(define (KI . rodalerfu)
  `(KI ,(apply string-append rodalerfu)))

(define (KOhA . rodalerfu)
  `(KOhA ,(apply string-append rodalerfu)))

(define (KU . rodalerfu)
  `(KU ,(apply string-append rodalerfu)))

(define (KUhE . rodalerfu)
  `(KUhE ,(apply string-append rodalerfu)))

(define (KUhO . rodalerfu)
  `(KUhO ,(apply string-append rodalerfu)))

(define (LA . rodalerfu)
  `(LA ,(apply string-append rodalerfu)))

(define (LAU . rodalerfu)
  `(LAU ,(apply string-append rodalerfu)))

(define (LAhE . rodalerfu)
  `(LAhE ,(apply string-append rodalerfu)))

(define (LE . rodalerfu)
  `(LE ,(apply string-append rodalerfu)))

(define (LEhU . rodalerfu)
  `(LEhU ,(apply string-append rodalerfu)))

(define (LI . rodalerfu)
  `(LI ,(apply string-append rodalerfu)))

(define (LIhU . rodalerfu)
  `(LIhU ,(apply string-append rodalerfu)))

(define (LOhO . rodalerfu)
  `(LOhO ,(apply string-append rodalerfu)))

(define (LOhU . rodalerfu)
  `(LOhU ,(apply string-append rodalerfu)))

(define (LU . rodalerfu)
  `(LU ,(apply string-append rodalerfu)))

(define (LUhU . rodalerfu)
  `(LUhU ,(apply string-append rodalerfu)))

(define (MAhO . rodalerfu)
  `(MAhO ,(apply string-append rodalerfu)))

(define (MAI . rodalerfu)
  `(MAI ,(apply string-append rodalerfu)))

(define (ME . rodalerfu)
  `(ME ,(apply string-append rodalerfu)))

(define (MEhU . rodalerfu)
  `(MEhU ,(apply string-append rodalerfu)))

(define (MOhE . rodalerfu)
  `(MOhE ,(apply string-append rodalerfu)))

(define (MOhI . rodalerfu)
  `(MOhI ,(apply string-append rodalerfu)))

(define (MOI . rodalerfu)
  `(MOI ,(apply string-append rodalerfu)))

(define (NA . rodalerfu)
  `(NA ,(apply string-append rodalerfu)))

(define (NAI . rodalerfu)
  `(NAI ,(apply string-append rodalerfu)))

(define (NAhE . rodalerfu)
  `(NAhE ,(apply string-append rodalerfu)))

(define (NAhU . rodalerfu)
  `(NAhU ,(apply string-append rodalerfu)))

(define (NIhE . rodalerfu)
  `(NIhE ,(apply string-append rodalerfu)))

(define (NIhO . rodalerfu)
  `(NIhO ,(apply string-append rodalerfu)))

(define (NOI . rodalerfu)
  `(NOI ,(apply string-append rodalerfu)))

(define (NU . rodalerfu)
  `(NU ,(apply string-append rodalerfu)))

(define (NUhA . rodalerfu)
  `(NUhA ,(apply string-append rodalerfu)))

(define (NUhI . rodalerfu)
  `(NUhI ,(apply string-append rodalerfu)))

(define (NUhU . rodalerfu)
  `(NUhU ,(apply string-append rodalerfu)))

(define (PA . rodalerfu)
  `(PA ,(apply string-append rodalerfu)))

(define (PEhE . rodalerfu)
  `(PEhE ,(apply string-append rodalerfu)))

(define (PEhO . rodalerfu)
  `(PEhO ,(apply string-append rodalerfu)))

(define (PU . rodalerfu)
  `(PU ,(apply string-append rodalerfu)))

(define (RAhO . rodalerfu)
  `(RAhO ,(apply string-append rodalerfu)))

(define (ROI . rodalerfu)
  `(ROI ,(apply string-append rodalerfu)))

(define (SA . rodalerfu)
  `(SA ,(apply string-append rodalerfu)))

(define (SE . rodalerfu)
  `(SE ,(apply string-append rodalerfu)))

(define (SEI . rodalerfu)
  `(SEI ,(apply string-append rodalerfu)))

(define (SEhU . rodalerfu)
  `(SEhU ,(apply string-append rodalerfu)))

(define (SI . rodalerfu)
  `(SI ,(apply string-append rodalerfu)))

(define (SOI . rodalerfu)
  `(SOI ,(apply string-append rodalerfu)))

(define (SU . rodalerfu)
  `(SU ,(apply string-append rodalerfu)))

(define (TAhE . rodalerfu)
  `(TAhE ,(apply string-append rodalerfu)))

(define (TEhU . rodalerfu)
  `(TEhU ,(apply string-append rodalerfu)))

(define (TEI . rodalerfu)
  `(TEI ,(apply string-append rodalerfu)))

(define (TO . rodalerfu)
  `(TO ,(apply string-append rodalerfu)))

(define (TOI . rodalerfu)
  `(TOI ,(apply string-append rodalerfu)))

(define (TUhE . rodalerfu)
  `(TUhE ,(apply string-append rodalerfu)))

(define (TUhU . rodalerfu)
  `(TUhU ,(apply string-append rodalerfu)))

(define (UI . rodalerfu)
  `(UI ,(apply string-append rodalerfu)))

(define (VA . rodalerfu)
  `(VA ,(apply string-append rodalerfu)))

(define (VAU . rodalerfu)
  `(VAU ,(apply string-append rodalerfu)))

(define (VEI . rodalerfu)
  `(VEI ,(apply string-append rodalerfu)))

(define (VEhO . rodalerfu)
  `(VEhO ,(apply string-append rodalerfu)))

(define (VEhA . rodalerfu)
  `(VEhA ,(apply string-append rodalerfu)))

(define (VIhA . rodalerfu)
  `(VIhA ,(apply string-append rodalerfu)))

(define (VUhO . rodalerfu)
  `(VUhO ,(apply string-append rodalerfu)))

(define (VUhU . rodalerfu)
  `(VUhU ,(apply string-append rodalerfu)))

(define (XI . rodalerfu)
  `(XI ,(apply string-append rodalerfu)))

(define (Y . rodalerfu)
  `(Y ,(apply string-append rodalerfu)))

(define (ZAhO . rodalerfu)
  `(ZAhO ,(apply string-append rodalerfu)))

(define (ZEhA . rodalerfu)
  `(ZEhA ,(apply string-append rodalerfu)))

(define (ZEI . rodalerfu)
  `(ZEI ,(apply string-append rodalerfu)))

(define (ZI . rodalerfu)
  `(ZI ,(apply string-append rodalerfu)))

(define (ZIhE . rodalerfu)
  `(ZIhE ,(apply string-append rodalerfu)))

(define (ZO . rodalerfu)
  `(ZO ,(apply string-append rodalerfu)))

(define (ZOI . rodalerfu)
  `(ZOI ,(apply string-append rodalerfu)))

(define (ZOhU . rodalerfu)
  `(ZOhU ,(apply string-append rodalerfu)))


