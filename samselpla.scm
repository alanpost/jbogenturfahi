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

;;;
;;; Helper routines
;;;

;; if an optional rule was not matched, return null.
;; Otherwise return the rule.
;;
(define-syntax ?-null
  (syntax-rules ()
    ((_ jalge)
     (if (and (string? jalge) (string=? "" jalge))
         '()
         `(,jalge)))))

;; if a zero-or-more rule was not matched, return
;; null.  Otherwise return the rule.
;;
(define-syntax *-null
  (syntax-rules ()
    ((_ jalge)
     jalge)))

;; if an optional rule was not matched, ignore it rather than
;; returning the default value.
;;
(define-syntax ?-ignore
  (syntax-rules ()
    ((_ jalge)
     (if (and (string? jalge) (string=? "" jalge))
         secuxna-nastura
         jalge))))

;; if a zero-or-more rule was not matched, ignore it rather than
;; returning the default value.
;;
(define-syntax *-ignore
  (syntax-rules ()
    ((_ jalge)
     (if (null? jalge)
         secuxna-nastura
         jalge))))


;;;
;;; rafske
;;;

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
(define (CVCy-lujvo-brivla-core CVC-rafsi
                                y
                                h
                                initial-rafsi
                                brivla-core)
  (string-append CVC-rafsi
                 y
                 h
                 (apply string-append initial-rafsi)
                 brivla-core))

(define (CVCy-lujvo-short-final-rafsi stressed-CVC-rafsi y short-final-rafsi)
  (string-append stressed-CVC-rafsi y short-final-rafsi))

(define (cmavo-form onset nucleus-h nucleus)
  (string-append onset
                 (apply string-append (map-apply string-append nucleus-h))
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

(define (stressed-fuhivla-rafsi fuhivla-head stressed-syllable onset y)
  (string-append fuhivla-head stressed-syllable onset y))

(define (fuhivla-rafsi fuhivla-head onset y h)
  (string-append fuhivla-head onset y h))

(define (brivla-head unstressed-syllable)
  (apply string-append unstressed-syllable))

(define (slinkuhi consonant rafsi-string)
  (string-append consonant rafsi-string))

(define (rafsi-string y-less-rafsi rest)
  (string-append (apply string-append y-less-rafsi) rest))

(define (rafsi-string-short-final stressed-y-less-rafsi short-final-rafsi)
  (string-append stressed-y-less-rafsi short-final-rafsi))

(define (rafsi-string-initial-pair stressed-y-less-rafsi
                                   initial-pair
                                   y)
  (string-append stressed-y-less-rafsi initial-pair y))


;; gismu
;;
(define (gismu . rodalerfu)
  `(gismu ,(apply string-append rodalerfu)))

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

(define (stressed-CVV-rafsi consonant vowel r-hyphen)
  (string-append consonant vowel r-hyphen))

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

(define (CVV-rafsi consonant vowel r-hyphen)
  (string-append consonant vowel r-hyphen))

(define (CVV-rafsi-VhV unstressed-vowel0 h unstressed-vowel1)
  (string-append unstressed-vowel0 h unstressed-vowel1))


;; non-Lojban word
;;
(define (non-lojban-word rodalerfu)
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

; ji is the only two-letter cmavo in A.  The rest are
; a single letter.
;
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

(define (Y rodalerfu)
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


;;;
;;; gerna
;;;

(define (text intro-null
              NAI-clause*
              text-part-2
              joik-jek?
              text-1?
              faho-clause
              EOF?)
  `(,intro-null
    ,@(*-null NAI-clause*)
    ,text-part-2
    ,@(?-null joi-jek?)
    ,@(?-null text-1)
    ,faho-clause
    ,@(?-null EOF?)))

(define (intro-null su-clause* intro-si-clause)
  `(,@(*-null su-clause) ,intro-si-clause))

; |text-part| is either |CMENE-clause+| or |indicators?|
; |?-null| handles |CMENE-clause+|
;
(define (text-part-2 text-part free*
  ,(,@(?-null text-part) ,@(*-null free*))))

(define (intro-si-clause si-clause? SI-clause*)
  `(,@(?-null si-clause?) ,@(*-null SI-clause*)))

(define (faho-clause FAhO-clause?)
  `(,@(?-null FAhO-clause?)))

(define (text-1-I I-clause
                  jek-or-joik?
                  stag?-BO-clause?
                  free*
                  text-1?)
  `(,I-clause
    ,@(?-null jek-or-joik?)
    ,@(apply (lambda (stag? BO-clause?)
               `(,@(?-null stag?) ,@(?-null BO-clause?)))
             stag?-BO-clause)
    ,@(*-null free*)
    ,@(?-null text-1?)))

(define (text-1-NIhO NIhO-clause+ free* su-clause* paragraphs?)
  `(,@NIhO-clause+
    ,@(*-null free*)
    ,@(*-null su-clause*)
    ,@(?-null paragraphs?)))

(define (paragraphs . rodasumti)
  `(paragraphs ,@rodasumti))

(define (paragraph . rodasumti)
  `(paragraph ,@rodasumti))

(define (statement . rodasumti)
  `(statement ,@rodasumti))


(define (sumti . rodasumti)
  `(sumti ,@rodasumti))

(define (sumti-BOI . rodasumti)
  `(sumti-BOI ,@rodasumti))

(define (sumti-LU . rodasumti)
  `(sumti-LU ,@rodasumti))

(define (sumti-LAhE . rodasumti)
  `(sumti-LAhE ,@rodasumti))

(define (sumti-KOhA . rodasumti)
  `(sumti-KOhA ,@rodasumti))

(define (sumti-LA . rodasumti)
  `(sumti-LA ,@rodasumti))

(define (sumti-LE . rodasumti)
  `(sumti-LE ,@rodasumti))


(define (li-clause . rodasumti)
  `(li-clause ,@rodasumti))


(define (selbri . rodasumti)
  `(selbri ,@rodasumti))

(define (selbri-NA . rodasumti)
  `(selbri-NA ,@rodasumti))


(define (tanru-unit . rodasumti)
  `(tanru-unit ,@rodasumti))


(define (tanru-unit-GOhA . rodasumti)
  `(tanru-unit-GOhA ,@rodasumti))

(define (tanru-unit-KE . rodasumti)
  `(tanru-unit-KE ,@rodasumti))

(define (tanru-unit-ME . rodasumti)
  `(tanru-unit-ME ,@rodasumti))

(define (tanru-unit-MOI . rodasumti)
  `(tanru-unit-MOI ,@rodasumti))

(define (tanru-unit-NUhA . rodasumti)
  `(tanru-unit-NUhA ,@rodasumti))

(define (tanru-unit-SE . rodasumti)
  `(tanru-unit-SE ,@rodasumti))

(define (tanru-unit-JAI . rodasumti)
  `(tanru-unit-JAI ,@rodasumti))

(define (tanru-unit-NAhE . rodasumti)
  `(tanru-unit-NAhE ,@rodasumti))

(define (tanru-unit-NU . rodasumti)
  `(tanru-unit-NU ,@rodasumti))


(define (linkargs . rodasumti)
  `(linkargs ,@rodasumti))

(define (linkargs-1 . rodasumti)
  `(linkargs-1 ,@rodasumti))

(define (linkargs-sa . rodasumti)
  `(linkargs-sa ,@rodasumti))


(define (links . rodasumti)
  `(links ,@rodasumti))

(define (links-1 . rodasumti)
  `(links-1 ,@rodasumti))

(define (links-sa . rodasumti)
  `(links-sa ,@rodasumti))


(define (quantifier-BOI . rodasumti)
  `(quantifier-BOI ,@rodasumti))

(define (quantifier-VEI . rodasumti)
  `(quantifier-VEI ,@rodasumti))


(define (mex . rodasumti)
  `(mex ,@rodasumti))


(define (operand-BOI . rodasumti)
  `(operand-BOI ,@rodasumti))


(define (number . rodasumti)
  `(number ,@rodasumti))

(define (lerfu-string . rodasumti)
  `(lerfu-string ,@rodasumti))


(define (free-SEI SEI-clause free* CU-terms? selbri SEhU-clause?)
  `(,SEI-clause ,@free* ,(?-null CU-terms?) ,selbri (?-null SEhU-clause?)))

(define (free-SOI SOI-clause free* sumti sumti? SEhU-clause?)
  `(,SOI-clause ,@free* ,sumti (?-null sumti?) (?-null SEhU-caluse?)))

(define (free-vocative-selbri vocative
                              relative-clauses-0?
                              selbri
                              relative-clauses-1?
                              DOhU-clause?)
  `(,vocative
    ,@(?-null relative-clauses-0?)
    ,selbri 
    ,@(?-null relative-clauses-1?)
    ,@(?-null DOhU-clause?)))

(define (free-vocative-cmene vocative
                             relative-clauses-0?
                             CMENE-clause+
                             free*
                             relative-clauses-1?
                             DOhU-clause?)
  `(,vocative
    ,@(?-null relative-clauses-0?)
    ,@CMENE-clause+
    ,@free*
    ,@(?-null relative-clauses-1?)
    ,@(?-null DOhU-clause?)))

(define (free-vocative-sumti vocative sumti? DOhU-clause?)
  `(,vocative ,@(?-null sumti?) ,@(?-null DOhU-clause?)))

(define (free-MAI number-or-lerfu-string MAI-clause)
  `(,number-or-lerfu-string ,MAI-clause))

(define (free-TO TO-clause text TOI-clause)
  `(,TO-clause ,text ,TOI-clause))


(define (xi-clause-BOI XI-clause free* number-or-lerfu-string BOI-clause?)
  `(,XI-clause ,@free* ,number-or-lerfu-string ,@(?-null BOI-clause?)))

(define (xi-clause-VEI XI-clause free* VEI-clause free* mex VEhO-clause?)
  `(,XI-clause ,@free* ,VEI-clause ,@free* ,mex ,@(?-null VEhO-clause?)))

(define (vocative COI-NAI?-clause* #!optional (DOI-clause? ""))
  (define (COI-NAI? COI-clause NAI-clause?)
    `(,COI-clause ,@(?-null NAI-clause)))

  `(,@(map-apply COI-NAI? COI-NAI?-clause*) ,@(?-null DOI-clause?)))

(define (indicators FUhE-clause? indicator+)
  `(,@(?-null FUhE-clause?) ,@indicator+))

(define (indicator clause #!optional (NAhE-clause? ""))
  `(,clause ,@(?-null NAhE-clause)))

(define (zei-clause . rodasumti)
  `(zei-clause ,@rodasumti))

(define (bu-clause . rodasumti)
  `(bu-clause ,@rodasumti))

(define (post-clause si-clause? indicators*)
  (let ((jalge `(,@(?-null si-clause?) ,@(*-null indicators*))))
    (*-ignore jalge)))

(define (pre-clause BAhE-clause?)
  (?-ignore BAhE-clause?))

(define (A-clause . rodasumti)
  `(A-clause ,@rodasumti))

(define (BAI-clause . rodasumti)
  `(BAI-clause ,@rodasumti))

(define (BAhE-clause . rodasumti)
  `(BAhE-clause ,@rodasumti))

(define (BE-clause . rodasumti)
  `(BE-clause ,@rodasumti))

(define (BEI-clause . rodasumti)
  `(BEI-clause ,@rodasumti))

(define (BEhO-clause . rodasumti)
  `(BEhO-clause ,@rodasumti))

(define (BIhE-clause . rodasumti)
  `(BIhE-clause ,@rodasumti))

(define (BIhI-clause . rodasumti)
  `(BIhI-clause ,@rodasumti))

(define (BO-clause . rodasumti)
  `(BO-clause ,@rodasumti))

(define (bOI-clause . rodasumti)
  `(bOI-clause ,@rodasumti))

(define (BU-clause . rodasumti)
  `(BU-clause ,@rodasumti))

(define (BY-clause . rodasumti)
  `(BY-clause ,@rodasumti))

(define (CAhA-clause . rodasumti)
  `(CAhA-clause ,@rodasumti))

(define (CAI-clause . rodasumti)
  `(CAI-clause ,@rodasumti))

(define (CEI-clause . rodasumti)
  `(CEI-clause ,@rodasumti))

(define (CEhE-clause . rodasumti)
  `(CEhE-clause ,@rodasumti))

(define (CO-clause . rodasumti)
  `(CO-clause ,@rodasumti))

(define (COI-clause . rodasumti)
  `(COI-clause ,@rodasumti))

(define (CU-clause . rodasumti)
  `(CU-clause ,@rodasumti))

(define (CUhE-clause . rodasumti)
  `(CUhE-clause ,@rodasumti))

(define (DAhO-clause . rodasumti)
  `(DAhO-clause ,@rodasumti))

(define (DOI-clause . rodasumti)
  `(DOI-clause ,@rodasumti))

(define (DOhU-clause . rodasumti)
  `(DOhU-clause ,@rodasumti))

(define (FA-clause . rodasumti)
  `(FA-clause ,@rodasumti))

(define (FAhA-clause . rodasumti)
  `(FAhA-clause ,@rodasumti))

(define (FAhO-clause . rodasumti)
  `(FAhO-clause ,@rodasumti))

(define (FEhE-clause . rodasumti)
  `(FEhE-clause ,@rodasumti))

(define (FEhU-clause . rodasumti)
  `(FEhU-clause ,@rodasumti))

(define (FIhO-clause . rodasumti)
  `(FIhO-clause ,@rodasumti))

(define (FOI-clause . rodasumti)
  `(FOI-clause ,@rodasumti))

(define (FUhA-clause . rodasumti)
  `(FUhA-clause ,@rodasumti))

(define (FUhE-clause . rodasumti)
  `(FUhE-clause ,@rodasumti))

(define (FUhO-clause . rodasumti)
  `(FUhO-clause ,@rodasumti))

(define (GA-clause . rodasumti)
  `(GA-clause ,@rodasumti))

(define (GAhO-clause . rodasumti)
  `(GAhO-clause ,@rodasumti))

(define (GEhU-clause . rodasumti)
  `(GEhU-clause ,@rodasumti))

(define (GI-clause . rodasumti)
  `(GI-clause ,@rodasumti))

(define (GIhA-clause . rodasumti)
  `(GIhA-clause ,@rodasumti))

(define (GOI-clause . rodasumti)
  `(GOI-clause ,@rodasumti))

(define (GOhA-clause . rodasumti)
  `(GOhA-clause ,@rodasumti))

(define (GUhA-clause . rodasumti)
  `(GUhA-clause ,@rodasumti))

(define (I-clause . rodasumti)
  `(I-clause ,@rodasumti))

(define (JA-clause . rodasumti)
  `(JA-clause ,@rodasumti))

(define (JAI-clause . rodasumti)
  `(JAI-clause ,@rodasumti))

(define (JOhI-clause . rodasumti)
  `(JOhI-clause ,@rodasumti))

(define (JOI-clause . rodasumti)
  `(JOI-clause ,@rodasumti))

(define (KE-clause . rodasumti)
  `(KE-clause ,@rodasumti))

(define (KEhE-clause . rodasumti)
  `(KEhE-clause ,@rodasumti))

(define (KEI-clause . rodasumti)
  `(KEI-clause ,@rodasumti))

(define (KI-clause . rodasumti)
  `(KI-clause ,@rodasumti))

(define (KOhA-clause . rodasumti)
  `(KOhA-clause ,@rodasumti))

(define (KU-clause . rodasumti)
  `(KU-clause ,@rodasumti))

(define (KUhE-clause . rodasumti)
  `(KUhE-clause ,@rodasumti))

(define (KUhO-clause . rodasumti)
  `(KUhO-clause ,@rodasumti))

(define (LA-clause . rodasumti)
  `(LA-clause ,@rodasumti))

(define (LAU-clause . rodasumti)
  `(LAU-clause ,@rodasumti))

(define (LAhE-clause . rodasumti)
  `(LAhE-clause ,@rodasumti))

(define (LE-clause . rodasumti)
  `(LE-clause ,@rodasumti))

(define (LEhU-clause . rodasumti)
  `(LEhU-clause ,@rodasumti))

(define (LI-clause . rodasumti)
  `(LI-clause ,@rodasumti))

(define (LIhU-clause . rodasumti)
  `(LIhU-clause ,@rodasumti))

(define (LOhO-clause . rodasumti)
  `(LOhO-clause ,@rodasumti))

(define (LOhU-clause . rodasumti)
  `(LOhU-clause ,@rodasumti))

(define (LU-clause . rodasumti)
  `(LU-clause ,@rodasumti))

(define (LUhU-clause . rodasumti)
  `(LUhU-clause ,@rodasumti))

(define (MAhO-clause . rodasumti)
  `(MAhO-clause ,@rodasumti))

(define (MAI-clause . rodasumti)
  `(MAI-clause ,@rodasumti))

(define (ME-clause . rodasumti)
  `(ME-clause ,@rodasumti))

(define (MEhU-clause . rodasumti)
  `(MEhU-clause ,@rodasumti))

(define (MOhE-clause . rodasumti)
  `(MOhE-clause ,@rodasumti))

(define (MOhI-clause . rodasumti)
  `(MOhI-clause ,@rodasumti))

(define (MOI-clause . rodasumti)
  `(MOI-clause ,@rodasumti))

(define (NA-clause . rodasumti)
  `(NA-clause ,@rodasumti))

(define (NAI-clause . rodasumti)
  `(NAI-clause ,@rodasumti))

(define (NAhE-clause . rodasumti)
  `(NAhE-clause ,@rodasumti))

(define (NAhU-clause . rodasumti)
  `(NAhU-clause ,@rodasumti))

(define (NIhE-clause . rodasumti)
  `(NIhE-clause ,@rodasumti))

(define (NIhO-clause . rodasumti)
  `(NIhO-clause ,@rodasumti))

(define (NOI-clause . rodasumti)
  `(NOI-clause ,@rodasumti))

(define (NU-clause . rodasumti)
  `(NU-clause ,@rodasumti))

(define (NUhA-clause . rodasumti)
  `(NUhA-clause ,@rodasumti))

(define (NUhI-clause . rodasumti)
  `(NUhI-clause ,@rodasumti))

(define (NUhU-clause . rodasumti)
  `(NUhU-clause ,@rodasumti))

(define (PA-clause . rodasumti)
  `(PA-clause ,@rodasumti))

(define (PEhE-clause . rodasumti)
  `(PEhE-clause ,@rodasumti))

(define (PEhO-clause . rodasumti)
  `(PEhO-clause ,@rodasumti))

(define (PU-clause . rodasumti)
  `(PU-clause ,@rodasumti))

(define (RAhO-clause . rodasumti)
  `(RAhO-clause ,@rodasumti))

(define (ROI-clause . rodasumti)
  `(ROI-clause ,@rodasumti))

(define (SA-clause . rodasumti)
  `(SA-clause ,@rodasumti))

(define (SE-clause . rodasumti)
  `(SE-clause ,@rodasumti))

(define (SEI-clause . rodasumti)
  `(SEI-clause ,@rodasumti))

(define (SEhU-clause . rodasumti)
  `(SEhU-clause ,@rodasumti))

(define (SI-clause . rodasumti)
  `(SI-clause ,@rodasumti))

(define (SOI-clause . rodasumti)
  `(SOI-clause ,@rodasumti))

(define (SU-clause . rodasumti)
  `(SU-clause ,@rodasumti))

(define (TAhE-clause . rodasumti)
  `(TAhE-clause ,@rodasumti))

(define (TEhU-clause . rodasumti)
  `(TEhU-clause ,@rodasumti))

(define (TEI-clause . rodasumti)
  `(TEI-clause ,@rodasumti))

(define (TO-clause . rodasumti)
  `(TO-clause ,@rodasumti))

(define (TOI-clause . rodasumti)
  `(TOI-clause ,@rodasumti))

(define (TUhE-clause . rodasumti)
  `(TUhE-clause ,@rodasumti))

(define (TUhU-clause . rodasumti)
  `(TUhU-clause ,@rodasumti))

(define (UI-clause . rodasumti)
  `(UI-clause ,@rodasumti))

(define (VA-clause . rodasumti)
  `(VA-clause ,@rodasumti))

(define (VAU-clause . rodasumti)
  `(VAU-clause ,@rodasumti))

(define (VEI-clause . rodasumti)
  `(VEI-clause ,@rodasumti))

(define (VEhO-clause . rodasumti)
  `(VEhO-clause ,@rodasumti))

(define (VUhU-clause . rodasumti)
  `(VUhU-clause ,@rodasumti))

(define (VEhA-clause . rodasumti)
  `(VEhA-clause ,@rodasumti))

(define (VIhA-clause . rodasumti)
  `(VIhA-clause ,@rodasumti))

(define (VUhO-clause . rodasumti)
  `(VUhO-clause ,@rodasumti))

(define (XI-clause . rodasumti)
  `(XI-clause ,@rodasumti))

;(define (Y-clause . rodasumti)
;  `(Y-clause ,@rodasumti))

(define (ZAhO-clause . rodasumti)
  `(ZAhO-clause ,@rodasumti))

(define (ZEhA-clause . rodasumti)
  `(ZEhA-clause ,@rodasumti))

(define (ZEI-clause . rodasumti)
  `(ZEI-clause ,@rodasumti))

(define (ZI-clause . rodasumti)
  `(ZI-clause ,@rodasumti))

(define (ZIhE-clause . rodasumti)
  `(ZIhE-clause ,@rodasumti))

(define (ZO-clause . rodasumti)
  `(ZO-clause ,@rodasumti))

(define (ZOI-clause ZOI-pre ZOI-post)
  `(ZOI-clause ,@ZOI-pre ,ZOI-post))

(define (ZOhU-clause . rodasumti)
  `(ZOhU-clause ,@rodasumti))

;; zoi
;;
(define-values (zoi-open zoi-word zoi-close)
  (let ((zoi '()))
    (values
      (lambda (any-word)
        (set! zoi any-word)
        any-word)
      
      (lambda (any-word)
        (if (not (equal=? zoi any-word))
            any-word
            #f))

      (lambda (any-word)
        (if (equal=? zoi any-word)
            any-word
            #f)))))
