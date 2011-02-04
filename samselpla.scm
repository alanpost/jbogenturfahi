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

(define-syntax ?*-null
  (syntax-rules ()
    ((_ jalge)
     (if (and (string? jalge) (string=? "" jalge))
         '()
         jalge))))

(define-syntax ?+-null
  (syntax-rules ()
    ((_ jalge)
     (if (and (string? jalge) (string=? "" jalge))
         '()
         jalge))))

;; if a zero-or-more rule was not matched, return
;; null.  Otherwise return the rule.
;;
(define-syntax *-null
  (syntax-rules ()
    ((_ jalge)
     jalge)))

;; if a one-or-more rule was matched.
;;
(define-syntax +-null
  (syntax-rules ()
    ((_ jalge)
     jalge)))


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
  `(cmene ,(string-append (apply string-append nafanmo) consonant)))

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
  `(text
    ,@intro-null
    ,@(*-null NAI-clause*)
    ,@text-part-2
    ,@(?-null joik-jek?)
    ,@(?-null text-1?)
    ,@faho-clause
    ,@(?*-null EOF?)))

(define (intro-null su-clause* intro-si-clause)
  `(,@(*-null su-clause*) ,@intro-si-clause))

; |text-part| is either |CMENE-clause+| or |indicators?|
; |?-null| handles |CMENE-clause+|
;
(define (text-part-2 text-part free*)
  `(,@(?-null text-part) ,@(*-null free*)))

(define (intro-si-clause si-clause? SI-clause*)
  `(,@(?-null si-clause?) ,@(*-null SI-clause*)))

(define (faho-clause FAhO-clause?)
  (?-null FAhO-clause?))

(define (text-1-I I-clause
                  jek-or-joik?
                  stag?
                  BO-clause?
                  free*
                  text-1?)
  `(,@I-clause
    ,@(?-null jek-or-joik?)
    ,@(?-null stag?)
    ,@(?-null BO-clause?)
    ,@(*-null free*)
    ,@(?-null text-1?)))

(define (text-1-NIhO NIhO-clause+ free* su-clause* paragraphs?)
  `(,@(+-null NIhO-clause+)
    ,@(*-null free*)
    ,@(*-null su-clause*)
    ,@(?-null paragraphs?)))

(define (text-1-paragraphs paragraphs)
  paragraphs)

;; NIhO-clause? is: ( NIhO-clause+ free* su-clause* paragraphs )?
;; ?-null handles + and *.
;;
(define (paragraphs paragraph NIhO-clause+ free* su-clause* paragraphs)
  `(paragraphs
    ,@paragraph
    ,@(?*-null NIhO-clause+)
    ,@(?*-null free*)
    ,@(?*-null su-clause*)
    ,@(?-null paragraphs)))

(define (paragraph statement-or-fragment I-clause*)
  (define (I I-clause free* statement-or-fragment?)
    `(,@I-clause ,@(*-null free*) ,@(?-null statement-or-fragment?)))

  `((paragraph ,@statement-or-fragment ,@(map-apply I I-clause*))))

(define (statement . rodasumti)
  rodasumti)

(define (statement-1 statement-2 I-clause*)
  (define (I I-clause joik-jek statement-2?)
    `(,@I-clause ,@joik-jek ,@(?-null statement-2?)))

  `(,@statement-2 ,@(map-apply I I-clause*)))

(define (statement-2 statement-3
                     I-clause?
                     jek-or-joik?
                     stag?
                     BO-clause?
                     free*
                     #!optional (statement-2? ""))
  `(,@statement-3
    ,@(?-null I-clause?)
    ,@(?-null jek-or-joik?)
    ,@(?-null stag?)
    ,@(?-null BO-clause?)
    ,@(?*-null free*)
    ,@(?-null statement-2?)))

(define (statement-3 sentence)
  sentence)

(define (statement-3-TUhE tag? TUhE-clause free* text-1 TUhU-clause? free*)
  `(,@(?-null tag?)
    ,@TUhE-clause
    ,@(*-null free*)
    ,@text-1
    ,@(?-null TUhU-clause?)
    ,@(*-null free*)))

(define (fragment-prenex prenex)
  prenex)

(define (fragment-terms terms VAU-clause? free*)
  `(,@(+-null terms) ,@(?-null VAU-clause?) ,@(*-null free*)))

(define (fragment-ek ek free*)
  `(,@ek ,@(*-null free*)))

(define (fragment-gihek gihek free*)
  `(,@gihek ,@(*-null free*)))

(define (fragment-quantifier quantifier)
  quantifier)

(define (fragment-NA NA-clause free*)
  `(,@NA-clause ,@(*-null free*)))

(define (fragment-relative-clauses relative-clauses)
  relative-clauses)

(define (fragment-links links)
  links)

(define (fragment-linkargs linkargs)
  linkargs)

(define (prenex . rodasumti)
  `(prenex ,@rodasumti))

(define (sentence terms?
                  bridi-tail-sa?*
                  CU-clause?
                  free?*
                  bridi-tail-sa*
                  bridi-tail)
  `(sentence ,@(?+-null terms?)
             ,@(?*-null bridi-tail-sa?*)
             ,@(?-null CU-clause?)
             ,@(?*-null free?*)
             ,@(*-null bridi-tail-sa*)
             ,@bridi-tail))


(define (sentence-sa sentence-start sa-word* SA-clause)
  `(,@sentence-start ,@(*-null sa-word*) ,@SA-clause))

(define (sentence-start-I I-pre)
  I-pre)

(define (sentence-start-NIhO NIhO-pre)
  NIhO-pre)

(define (subsentence sentence)
  sentence)

(define (subsentence-prenex prenex subsentence)
  `(,@prenex ,@subsentence))

(define (bridi-tail bridi-tail-1
                    gihek?
                    stag?
                    KE-clause?
                    free-0?*
                    bridi-tail?
                    KEhE-clause?
                    free-1?*
                    tail-terms?)
  `(,@bridi-tail-1
    ,@(?-null gihek?)
    ,@(?-null stag?)
    ,@(?-null KE-clause?)
    ,@(?*-null free-0?*)
    ,@(?-null bridi-tail?)
    ,@(?-null KEhE-clause?)
    ,@(?*-null free-1?*)
    ,@(?-null tail-terms?)))


(define (bridi-tail-sa . rodasumti)
  `(bridi-tail-sa ,@rodasumti))


(define (bridi-tail-start-ME ME-clause)
  ME-clause)

(define (bridi-tail-start-NUhA NUhA-clause)
  NUhA-clause)

(define (bridi-tail-start-NU NU-clause)
  NU-clause)

(define (bridi-tail-start-NA NA-clause)
  NA-clause)

(define (bridi-tail-start-NAhE NAhE-clause)
  NAhE-clause)

(define (bridi-tail-start-selbri selbri)
  selbri)

(define (bridi-tail-start-tag tag bridi-tail-start)
  `(,@tag ,@bridi-tail-start))

(define (bridi-tail-start-KE KE-clause bridi-tail-start)
  `(,@KE-clause ,@bridi-tail-start))

(define (bridi-tail-start bridi-tail)
  bridi-tail)


(define (bridi-tail-1 bridi-tail-2 rest*)
  (define (rest gihek free* bridi-tail-2 tail-terms)
    `(,@gihek ,@(*-null free*) ,@bridi-tail-2 ,@tail-terms))

  `(,@bridi-tail-2 ,@(map-apply rest rest*)))

(define (bridi-tail-2 bridi-tail-3
                      gihek?
                      stag?
                      BO-clause?
                      free?*
                      bridi-tail-2?
                      tail-terms?)
  `(,@bridi-tail-3
    ,@(?-null gihek?)
    ,@(?-null stag?)
    ,@(?-null BO-clause?)
    ,@(?*-null free?*)
    ,@(?-null bridi-tail-2?)
    ,@(?-null tail-terms?)))


(define (bridi-tail-3-selbri selbri tail-terms)
  `(,@selbri ,@tail-terms))

(define (bridi-tail-3-gek gek-sentence)
  gek-sentence)


(define (gek-sentence . rodasumti)
  `(gek-sentence ,@rodasumti))

(define (gek-sentence-KE . rodasumti)
  `(gek-sentence-KE ,@rodasumti))

(define (gek-sentence-NA . rodasumti)
  `(gek-sentence-NA ,@rodasumti))


(define (tail-terms terms? VAU-clause? free*)
  `(,@(?+-null terms?) ,@(?-null VAU-clause?) ,@(*-null free*)))


(define (terms terms-1+)
  terms-1+)

(define (terms-1 terms-2 rest*)
  (define (terms pehe-sa* PEhE-clause free* joik-jek terms-2)
    `(,@terms
      ,@(*-null pehe-sa*)
      ,@PEhE-clause
      ,@(*-null free*)
      ,@joik-jek
      ,@terms-2))

  `(,@terms-2 ,@(map-apply terms rest*)))

(define (terms-2 term rest*)
  (define (terms cehe-sa* CEhE-clause free* term )
    `(,@(*-null cehe-sa*) ,@CEhE-clause ,@(*-null free*) ,@term))

  `(,@term ,@(map-apply terms rest*)))


(define (pehe-sa PEhE-clause sa-word* SA-clause)
  `(,@PEhE-clause ,@(*-null sa-word*) ,@SA-clause))

(define (cehe-sa CEhE-clause sa-word* SA-clause)
  `(,@CEhE-clause ,@(*-null sa-word*) ,@SA-clause))


(define (term term-sa* term-1)
  `(term ,@(*-null term-sa*) ,@term-1))


(define (term-1-sumti sumti)
  sumti)

(define (term-1-FA tag-or-FA sumti-or-KU)
  `(,@tag-or-FA ,@sumti-or-KU))

(define (term-1-FA-tag tag)
  tag)

(define (term-1-FA-clause FA-clause free*)
  `(,FA-clause ,@(*-null free*)))

(define (term-1-FA-sumti sumti)
  sumti)

(define (term-1-FA-KU KU-clause free*)
  `(,@KU-clause ,@(*-null free*)))

(define (term-1-termset termset)
  termset)

(define (term-1-NA NA-clause KU-clause free*)
  `(,@NA-clause ,@KU-clause ,@(*-null free*)))


(define (term-sa term-start sa-word* SA-clause)
  `(,@term-start ,@(*-null sa-word*) ,@SA-clause))


(define (term-start term-1)
  term-1)

(define (term-start-LA LA-clause)
  LA-clause)

(define (term-start-LE LE-clause)
  LE-clause)

(define (term-start-LI LI-clause)
  LI-clause)

(define (term-start-LU LU-clause)
  LU-clause)

(define (term-start-LAhE LAhE-clause)
  LAhE-clause)

(define (term-start-quantifier quantifier term-start)
  `(,@quantifier ,@term-start))

(define (term-start-gek gek sumti gik)
  `(,@gek ,@sumti ,@gik))

(define (term-start-FA FA-clause)
  FA-clause)

(define (term-start-tag tag term-start)
  `(,@tag ,@term-start))


(define (termset . rodasumti)
  `(termset ,@rodasumti))

(define (termset-gek . rodasumti)
  `(termset-gek ,@rodasumti))



(define (terms-gik-terms . rodasumti)
  `(terms-gik-terms ,@rodasumti))

(define (gek-termset . rodasumti)
  `(gek-termset ,@rodasumti))

(define (termset-terms . rodasumti)
  `(termset-terms ,@rodasumti))


(define (sumti sumti-1 VUhO-clause? free* relative-clauses?)
  `((sumti ,@sumti-1
           ,@(?-null VUhO-clause?)
           ,@(?*-null free*)
           ,@(?-null relative-clauses?))))

(define (sumti-1 sumti-2
                 joik-ek?
                 stag?
                 KE-clause?
                 free-0*
                 sumti?
                 KEhE-clause?
                 free-1*)
  `(,@sumti-2
    ,@(?-null joik-ek?)
    ,@(?-null stag?)
    ,@(?-null KE-clause?)
    ,@(?*-null free-0*)
    ,@(?-null sumti?)
    ,@(?-null KEhE-clause?)
    ,@(?*-null free-1*)))

(define (sumti-2 sumti-3 joik-ek-sumti-3*)
  (define (sumti joik-ek sumti-3)
    `(,@joik-ek ,@sumti-3))

  `(,@sumti-3 ,@(map-apply sumti joik-ek-sumti-3*)))

(define (sumti-3 sumti-4 joik-ek? stag? BO-clause? free* sumti-3?)
  `(,@sumti-4
    ,@(?-null joik-ek?)
    ,@(?-null stag?)
    ,@(?-null BO-clause?)
    ,@(?*-null free*)
    ,@(?-null sumti-3?)))

(define (sumti-4 sumti-5)
  sumti-5)

(define (sumti-4-gek . rodasumti)
  `(sumti-4-gek ,@rodasumti))

(define (sumti-5 quantifier? sumti-6 relative-clauses?)
  `(,@(?-null quantifier?) ,sumti-6 ,@(?-null relative-clauses?)))

(define (sumti-5-selbri quantifier selbri KU-clause? free* relative-clauses?)
  `(,@quantifier
    ,@selbri
    ,@(?-null KU-clause?)
    ,@(*-null free*)
    ,@(relative-clauses?)))

(define (sumti-6-ZO ZO-clause free*)
  `(,@ZO-clause ,@(*-null free*)))

(define (sumti-6-ZOI ZOI-clause free*)
  `(,@ZOI-clause ,@(*-null free*)))

(define (sumti-6-LOhU LOhU-clause free*)
  `(,@LOhU-clause ,@(*-null free*)))

(define (sumti-6-BOI lerfu-string BOI-clause? free*)
  `(,@lerfu-string ,@(?-null BOI-clause?) ,@(*-null free*)))

(define (sumti-6-LU LU-clause text LIhU-clause? free*)
  `(,@LU-clause ,text ,@(?-null LIhU-clause?) ,@(*-null free*)))

(define (sumti-6-LAhE-clause LAhE-clause free*)
  `(,@LAhE-clause ,@(*-null free*)))

(define (sumti-6-LAhE-NAhE NAhE-clause BO-clause free*)
  `(,@NAhE-clause ,@BO-clause ,@(*-null free*)))

(define (sumti-6-LAhE clause relative-clauses? sumti LUhU-clause? free*)
  `(,@clause
    ,@(?-null relative-clauses?)
    ,@sumti
    ,@(?-null LUhU-clause?)
    ,@(*-null free*)))

(define (sumti-6-KOhA KOhA-clause free*)
  `(,@KOhA-clause ,@(*-null free*)))

(define (sumti-6-LA LA-clause
                    free-0*
                    relative-clauses?
                    CMENE-clause+
                    free-1*)
  `(,@LA-clause
    ,@(*-null free-0*)
    ,@(?-null relative-clauses?)
    ,@CMENE-clause+
    ,@(*-null free-1*)))

(define (sumti-6-LE LA-or-LE-clause
                    free-0*
                    sumti-tail
                    KU-clause?
                    free-1*)
  `(,@LA-or-LE-clause
    ,@(*-null free-0*)
    ,@sumti-tail
    ,@(?-null KU-clause?)
    ,@(*-null free-1*)))

(define (sumti-6-LI LI-clause
                    free-0*
                    mex
                    LOhO-clause?
                    free-1*)
  `(,@LI-clause
    ,@(*-null free-0*)
    ,@mex
    ,@(?-null LOhO-clause?)
    ,@(*-null free-1*)))

(define (sumti-tail-sumti-6 sumti-6? relative-clauses? sumti-tail-1)
  `(,@(?-null sumti-6?) ,@(?-null relative-clauses?) ,@sumti-tail-1))

(define (sumti-tail relative-clauses sumti-tail-1)
  `(,@relative-clauses ,@sumti-tail-1))

(define (sumti-tail-1-selbri quantifier? selbri relative-clauses?)
  `(,@(?-null quantifier?) ,@selbri ,@(?-null relative-clauses?)))

(define (sumti-tail-1-sumti quantifier sumti)
  `(,@quantifier ,@sumti))

(define (relative-clauses relative-clause rest*)
  (define (relative-clause* ZIhE-clause free* relative-clause)
    `(,@ZIhE-clause ,@(*-null free*) ,@relative-clause))

  `(,@relative-clause ,@(map-apply relative-clause* rest*)))

(define (relative-clause relative-clause-sa* relative-clause-1)
  `(,@relative-clause-sa* ,@relative-clause-1))

(define (relative-clause-sa relative-clause-start sa-word* SA-clause)
  `(,@relative-clause-start ,@(*-null sa-word*) ,@SA-clause))

(define (relative-clause-1-GOI GOI-clause
                               free-0*
                               term
                               GEhU-clause?
                               free-1*)
  `(,@GOI-clause
    ,@(*-null free-0*)
    ,term
    ,@(?-null GEhU-clause?)
    ,@(*-null free-1*)))

(define (relative-clause-1-NOI NOI-clause
                           free-0*
                           subsentence
                           KUhO-clause?
                           free-1*)
  `(,@NOI-clause
    ,@(*-null free-0*)
    ,@subsentence
    ,@(?-null KUhO-clause?)
    ,@(*-null free-1*)))

(define (relative-clause-start . rodasumti)
  `(relative-clause-start ,@rodasumti))


(define (selbri tag? selbri-1)
  `((selbri ,@(?-null tag?) ,@selbri-1)))

(define (selbri-1 selbri-2)
  selbri-2)

(define (selbri-1-NA NA-clause free* selbri)
  `((,@NA-clause ,@(*-null free*) ,@selbri)))

(define (selbri-2 selbri-3 CO-clause? free?* selbri-2?)
  `(,@selbri-3
    ,@(?-null CO-clause?)
    ,@(?*-null free?*)
    ,@(?-null selbri-2?)))

(define (selbri-3 selbri-4+)
  selbri-4+)

(define (selbri-4 selbri-5 rest)
  `(,@selbri-5 ,@rest))

(define (selbri-4-joik-jek joik-jek selbri-5)
  `(,@joik-jek ,@selbri-5))

(define (selbri-4-joik joik
                       stag?
                       KE-clause
                       free-0*
                       selbri-3
                       KEhE-clause?
                       free-1*)
  `(,@joik
    ,@(?-null stag?)
    ,@KE-clause
    ,@(*-null free-0*)
    ,@selbri-3
    ,@(?-null KEhE-clause?)
    ,@(*-null free-1*)))


(define (selbri-5 selbri-6 jek-or-joik? stag? BO-clause? free?* selbri-5?)
  `(,@selbri-6
    ,@(?-null jek-or-joik?)
    ,@(?-null stag?)
    ,@(?-null BO-clause?)
    ,@(?*-null free?*)
    ,@(?-null selbri-5?)))

(define (selbri-6 tanru-unit BO-clause? free?* selbri-6?)
  `(,@tanru-unit
    ,@(?-null BO-clause?)
    ,@(?*-null free?*)
    ,@(?-null selbri-6?)))

(define (selbri-6-NAhE NAhE-clause? free* guhek selbri gik selbri-6)
  `(,@(?-null NAhE-clause?)
    ,@(*-null free*)
    ,@guhek
    ,@selbri
    ,@gik
    ,@selbri-6))


(define (tanru-unit tanru-unit-1 rest*)
  (define (rest CEI-clause free* tanru-unit-1)
    `(,@CEI-clause ,@(*-null free*) ,@tanru-unit-1))

  `(,@tanru-unit-1 ,@(map-apply rest rest*)))

(define (tanru-unit-1 tanru-unit-2 linkargs?)
  `(,@tanru-unit-2 ,@(?-null linkargs?)))


(define (tanru-unit-2-BRIVLA BRIVLA-clause free*)
  `(,@BRIVLA-clause ,@(*-null free*)))

(define (tanru-unit-2-GOhA GOhA-clause RAhO-clause? free*)
  `(,@GOhA-clause ,@(?-null RAhO-clause?) ,@(*-null free*)))

(define (tanru-unit-2-KE KE-clause
                         free-0*
                         selbri-3
                         KEhE-clause?
                         free-1*)
  `(,@KE-clause
    ,@(*-null free-0*)
    ,@selbri-3
    ,@(?-null KEhE-clause?)
    ,@(*-null free-1*)))

(define (tanru-unit-2-ME ME-clause
                         free-0*
                         sumti-or-lerfu-string
                         MEhU-clause?
                         free-1*
                         MOI-clause?
                         free-2*)
  `(,@ME-clause
    ,@(*-null free-0*)
    ,@sumti-or-lerfu-string
    ,@(?-null MEhU-clause?)
    ,@(*-null free-1*)
    ,@(?-null MOI-clause?)
    ,@(*-null free-2*)))

(define (tanru-unit-2-MOI number-or-lerfu-string MOI-clause free*)
  `(,@number-or-lerfu-string ,@MOI-clause ,@(*-null free*)))

(define (tanru-unit-2-NUhA NUhA-clause free* mex-operator)
  `(,@NUhA-clause ,@(*-null free*) ,@mex-operator))

(define (tanru-unit-2-SE SE-clause free* tanru-unit-2)
  `(,@SE-clause ,@(*-null free*) ,tanru-unit-2))

(define (tanru-unit-2-JAI JAI-clause free* tag? tanru-unit-2)
  `(,@JAI-clause ,@(*-null free*) ,@(?-null tag?) ,tanru-unit-2))

(define (tanru-unit-2-NAhE NAhE-clause free* tanru-unit-2)
  `(,@NAhE-clause ,@(*-null free*) ,tanru-unit-2))

(define (tanru-unit-2-NU NU-clause
                         NAI-clause?
                         free-0*
                         joik-jek-NU*
                         subsentence
                         KEI-clause?
                         free-1*)
  (define (joik-jek-NU joik-jek NU-clause NAI-clause? free*)
    `(,@joik-jek ,@NU-clause ,@(?-null NAI-clause?) ,@(*-null free*)))

  `(,@NU-clause
    ,@(?-null NAI-clause)
    ,@(*-null free-0*)
    ,@(map-apply joik-jek-NU joik-jek-NU*)
    ,@subsentence
    ,@(?-null KEI-clause?)
    ,@(*-null free-1*)))


(define (linkargs linkargs-sa* linkargs-1)
  `(,@(*-null linkargs-sa) ,@linkargs-1))

(define (linkargs-1 BE-clause free* term links? BEhO-clause? free*)
  `(,@BE-clause
    ,@(*-null free*)
    ,@term
    ,@(?-null links?)
    ,@(?-null BEhO-clause?)
    ,@(*-null free*)))

(define (linkargs-sa linkargs-start sa-word* SA-clause)
  `(,@linkargs-start ,@(*-null sa-word*) ,@SA-clause))

(define (linkargs-start BE-clause)
  BE-clause)

(define (links links-sa* links-1)
  `(,@(*-null links-sa*) ,@links-1))

(define (links-sa links-start sa-word* SA-clause)
  `(,@links-start ,@(*-null sa-word*) ,@SA-clause))

(define (links-1 BEI-clause free* term links?)
  `(,@BEI-clause ,@(*-null free*) ,@term ,@(?-null links?)))

(define (links-start BEI-clause)
  BEI-clause)

(define (quantifier-BOI . rodasumti)
  `(quantifier-BOI ,@rodasumti))

(define (quantifier-VEI . rodasumti)
  `(quantifier-VEI ,@rodasumti))

(define (mex . rodasumti)
  `(mex ,@rodasumti))

(define (mex-sa . rodasumti)
  `(mex-sa ,@rodasumti))

(define (mex-0 . rodasumti)
  `(mex-0 ,@rodasumti))

(define (mex-0-rp . rodasumti)
  `(mex-0-rp ,@rodasumti))

(define (mex-start-FUhA . rodasumti)
  `(mex-start-FUhA ,@rodasumti))

(define (mex-start-PEhO . rodasumti)
  `(mex-start-PEhO ,@rodasumti))

(define (mex-start . rodasumti)
  `(mex-start ,@rodasumti))

(define (rp-clause . rodasumti)
  `(rp-clause ,@rodasumti))

(define (mex-1 . rodasumti)
  `(mex-1 ,@rodasumti))

(define (mex-2 . rodasumti)
  `(mex-2 ,@rodasumti))

(define (mex-forethought . rodasumti)
  `(mex-forethought ,@rodasumti))

(define (fore-operands . rodasumti)
  `(fore-operands ,@rodasumti))

(define (rp-expression . rodasumti)
  `(rp-expression ,@rodasumti))

(define (rp-expression-tail . rodasumti)
  `(rp-expression-tail ,@rodasumti))

(define (operator . rodasumti)
  `(operator ,@rodasumti))

(define (operator-0 . rodasumti)
  `(operator-0 ,@rodasumti))

(define (operator-0-joik-jek . rodasumti)
  `(operator-0-joik-jek ,@rodasumti))

(define (operator-0-joik . rodasumti)
  `(operator-0-joik ,@rodasumti))

(define (operator-sa . rodasumti)
  `(operator-sa ,@rodasumti))

(define (operator-start . rodasumti)
  `(operator-start ,@rodasumti))

(define (operator-start-KE . rodasumti)
  `(operator-start-KE ,@rodasumti))

(define (operator-start-NAhE . rodasumti)
  `(operator-start-NAhE ,@rodasumti))

(define (operator-start-MAhO . rodasumti)
  `(operator-start-MAhO ,@rodasumti))

(define (operator-start-VUhU . rodasumti)
  `(operator-start-VUhU ,@rodasumti))

(define (operator-1 . rodasumti)
  `(operator-1 ,@rodasumti))

(define (operator-gukek . rodasumti)
  `(operator-gukek ,@rodasumti))

(define (operator-jek . rodasumti)
  `(operator-jek ,@rodasumti))

(define (operator-2 . rodasumti)
  `(operator-2 ,@rodasumti))

(define (operator-2-KE . rodasumti)
  `(operator-2-KE ,@rodasumti))

(define (mex-operator . rodasumti)
  `(mex-operator ,@rodasumti))

(define (mex-operator-NAhE . rodasumti)
  `(mex-operator-NAhE ,@rodasumti))

(define (mex-operator-MAhO . rodasumti)
  `(mex-operator-MAhO ,@rodasumti))

(define (mex-operator-NAhU . rodasumti)
  `(mex-operator-NAhU ,@rodasumti))

(define (mex-operator-VUhU . rodasumti)
  `(mex-operator-VUhU ,@rodasumti))

(define (operand . rodasumti)
  `(operand ,@rodasumti))

(define (operand-sa . rodasumti)
  `(operand-sa ,@rodasumti))

(define (operand-0 . rodasumti)
  `(operand-0 ,@rodasumti))

(define (operand-start-quantifier . rodasumti)
  `(operand-start-quantifier ,@rodasumti))

(define (operand-start-lerfu-word . rodasumti)
  `(operand-start-lerfu-word ,@rodasumti))

(define (operand-start-NIhE . rodasumti)
  `(operand-start-NIhE ,@rodasumti))

(define (operand-start-MOhE . rodasumti)
  `(operand-start-MOhE ,@rodasumti))

(define (operand-start-JOhI . rodasumti)
  `(operand-start-JOhI ,@rodasumti))

(define (operand-start-gek . rodasumti)
  `(operand-start-gek ,@rodasumti))

(define (operand-start-LAhE . rodasumti)
  `(operand-start-LAhE ,@rodasumti))

(define (operand-start-NAhE . rodasumti)
  `(operand-start-NAhE ,@rodasumti))

(define (operand-1 . rodasumti)
  `(operand-1 ,@rodasumti))

(define (operand-2 . rodasumti)
  `(operand-2 ,@rodasumti))

(define (operand-3 . rodasumti)
  `(operand-3 ,@rodasumti))

(define (operand-3-BOI . rodasumti)
  `(operand-3-BOI ,@rodasumti))

(define (operand-3-NIhE . rodasumti)
  `(operand-3-NIhE ,@rodasumti))

(define (operand-3-MOhE . rodasumti)
  `(operand-3-MOhE ,@rodasumti))

(define (operand-3-JOhI . rodasumti)
  `(operand-3-JOhI ,@rodasumti))

(define (operand-3-gek . rodasumti)
  `(operand-3-gek ,@rodasumti))

(define (operand-3-LAhE . rodasumti)
  `(operand-3-LAhE ,@rodasumti))

(define (operand-3-NAhE . rodasumti)
  `(operand-3-NAhE ,@rodasumti))
(define (operand-BOI . rodasumti)
  `(operand-BOI ,@rodasumti))

(define (number . rodasumti)
  `(number ,@rodasumti))

(define (lerfu-string . rodasumti)
  `(lerfu-string ,@rodasumti))

(define (lerfu-word-BY . rodasumti)
  `(lerfu-word-BY ,@rodasumti))

(define (lerfu-word-LAU . rodasumti)
  `(lerfu-word-LAU ,@rodasumti))

(define (lerfu-word-TEI . rodasumti)
  `(lerfu-word-TEI ,@rodasumti))

(define (ek NA-clause? SE-clause? A-clause NAI-clause?)
  `(,@(?-null NA-clause?)
    ,@(?-null SE-clause?)
    ,@A-clause
    ,@(?-null NAI-clause?)))

(define (gihek . rodasumti)
  `(gihek ,@rodasumti))

(define (gihek-sa . rodasumti)
  `(gihek-sa ,@rodasumti))

(define (gihek-1 . rodasumti)
  `(gihek-1 ,@rodasumti))

(define (jek . rodasumti)
  `(jek ,@rodasumti))

(define (joik-JOI . rodasumti)
  `(joik-JOI ,@rodasumti))

(define (joik-interval . rodasumti)
  `(joik-interval ,@rodasumti))

(define (joik-GAhO . rodasumti)
  `(joik-GAhO ,@rodasumti))

(define (interval . rodasumti)
  `(interval ,@rodasumti))

(define (joik-ek . rodasumti)
  `(joik-ek ,@rodasumti))

(define (joik-ek-sa . rodasumti)
  `(joik-ek-sa ,@rodasumti))

(define (joik-ek-1-joik . rodasumti)
  `(joik-ek-1-joik ,@rodasumti))

(define (joik-ek-1-ek . rodasumti)
  `(joik-ek-1-ek ,@rodasumti))

(define (joik-jek . rodasumti)
  `(joik-jek ,@rodasumti))

(define (gek-GA . rodasumti)
  `(gek-GA ,@rodasumti))

(define (gek-GI . rodasumti)
  `(gek-GI ,@rodasumti))

(define (gek-gik . rodasumti)
  `(gek-gik ,@rodasumti))

(define (guhek . rodasumti)
  `(guhek ,@rodasumti))

(define (gik . rodasumti)
  `(gik ,@rodasumti))

(define (tag . rodasumti)
  `(tag ,@rodasumti))

(define (stag-tense . rodasumti)
  `(stag-tense ,@rodasumti))

(define (stag-simple-tense . rodasumti)
  `(stag-simple-tense ,@rodasumti))

(define (tense-modal-simple-tense . rodasumti)
  `(tense-modal-simple-tense ,@rodasumti))

(define (tense-modal-FIhO . rodasumti)
  `(tense-modal-FIhO ,@rodasumti))

(define (simple-tense-modal-BAI . rodasumti)
  `(simple-tense-modal-BAI ,@rodasumti))

(define (simple-tense-modal-CUhE . rodasumti)
  `(simple-tense-modal-CUhE ,@rodasumti))

(define (simple-tense-modal-KI . rodasumti)
  `(simple-tense-modal-KI ,@rodasumti))

(define (simple-tense-modal-CAhA . rodasumti)
  `(simple-tense-modal-CAhA ,@rodasumti))

(define (time-ZI . rodasumti)
  `(time ,@rodasumti))

(define (time-offset . rodasumti)
  `(time-offset ,@rodasumti))

(define (space . rodasumti)
  `(space ,@rodasumti))

(define (space-offset . rodasumti)
  `(space-offset ,@rodasumti))

(define (space-interval . rodasumti)
  `(space-interval ,@rodasumti))

(define (space-interval-VEhA . rodasumti)
  `(space-interval-VEhA ,@rodasumti))

(define (space-int-props . rodasumti)
  `(space-int-props ,@rodasumti))

(define (interval-property . rodasumti)
  `(interval-property ,@rodasumti))

(define (interval-property-TAhE . rodasumti)
  `(interval-property-TAhE ,@rodasumti))

(define (interval-property-ZAhO . rodasumti)
  `(interval-property-ZAhO ,@rodasumti))

(define (free-SEI SEI-clause free* CU-terms? selbri SEhU-clause?)
  `(,@SEI-clause
    ,@(*-null free*)
    ,(?-null CU-terms?)
    ,@selbri
    (?-null SEhU-clause?)))

(define (free-SOI SOI-clause free* sumti sumti? SEhU-clause?)
  `(,@SOI-clause
    ,@(*-null free*)
    ,@sumti
    (?-null sumti?)
    (?-null SEhU-clause?)))

(define (free-vocative-selbri vocative
                              relative-clauses-0?
                              selbri
                              relative-clauses-1?
                              DOhU-clause?)
  `(,@vocative
    ,@(?-null relative-clauses-0?)
    ,@selbri
    ,@(?-null relative-clauses-1?)
    ,@(?-null DOhU-clause?)))

(define (free-vocative-cmene vocative
                             relative-clauses-0?
                             CMENE-clause+
                             free*
                             relative-clauses-1?
                             DOhU-clause?)
  `(,@vocative
    ,@(?-null relative-clauses-0?)
    ,@CMENE-clause+
    ,@(*-null free*)
    ,@(?-null relative-clauses-1?)
    ,@(?-null DOhU-clause?)))

(define (free-vocative-sumti vocative sumti? DOhU-clause?)
  `(,@vocative ,@(?-null sumti?) ,@(?-null DOhU-clause?)))

(define (free-MAI number-or-lerfu-string MAI-clause)
  `(,@number-or-lerfu-string ,@MAI-clause))

(define (free-TO TO-clause text TOI-clause)
  `(,@TO-clause ,text ,TOI-clause))


(define (xi-clause-BOI XI-clause free* number-or-lerfu-string BOI-clause?)
  `(,@XI-clause
    ,@(*-null free*)
    ,@number-or-lerfu-string
    ,@(?-null BOI-clause?)))

(define (xi-clause-VEI XI-clause free-0* VEI-clause free-1* mex VEhO-clause?)
  `(,@XI-clause
    ,@(*-null free-0*)
    ,@VEI-clause
    ,@(*-null free-1*)
    ,@mex
    ,@(?-null VEhO-clause?)))

;; XXX: I may want to flatten single-element * clauses, much
;;      like I do je rules.
;;
(define (vocative COI-NAI?-clause* #!optional (DOI-clause? ""))
  (define (COI-NAI? COI-clause NAI-clause?)
    `(,@COI-clause ,@(?-null NAI-clause?)))

  `(,@(apply append (map-apply COI-NAI? COI-NAI?-clause*))
    ,@(?-null DOI-clause?)))

(define (indicators FUhE-clause? indicator+)
  `(,@(?-null FUhE-clause?) ,@(apply append indicator+)))

(define (indicator clause #!optional (NAhE-clause? ""))
  `(,@clause ,@(?-null NAhE-clause?)))

(define (zei-clause . rodasumti)
  `(zei-clause ,@rodasumti))

(define (zei-clause-no-pre . rodasumti)
  `(zei-clause-no-pre ,@rodasumti))

(define (bu-clause . rodasumti)
  `(bu-clause ,@rodasumti))

(define (bu-clause-no-pre . rodasumti)
  `(bu-clause-no-pre ,@rodasumti))

(define (zei-tail . rodasumti)
  `(zei-tail ,@rodasumti))

(define (bu-tail . rodasumti)
  `(bu-tail ,@rodasumti))

(define (pre-zei-bu any-word si-clause?)
  `(,@any-word ,@(?-null si-clause?)))

(define (post-clause si-clause? indicators*)
  `(,@(?-null si-clause?) ,@(*-null indicators*)))

(define (pre-clause BAhE-clause?)
  (?-null BAhE-clause?))

(define (any-word-SA-LOhU LOhU-pre)
  `(any-string ,@LOhU-pre))

(define (any-word-SA-ZO ZO-pre)
  `(any-word ,@ZO-pre))

(define (any-word-SA-ZOI ZOI-pre)
  `(any-string ,@ZOI-pre))

(define (su-clause . rodasumti)
  `(su-clause ,@rodasumti))

(define (si-clause sumti+)
  (define (si clause si-clause? SI-clause)
    `(,@clause ,@(?-null si-clause?) ,@SI-clause))

  `(si-clause ,@(map-apply si sumti+)))

(define (erasable-clause-zei . rodasumti)
  `(erasable-clause .@rodasumti))

(define (erasable-clause-bu . rodasumti)
  `(erasable-clause .@rodasumti))

(define (sa-word pre-zei-bu)
  pre-zei-bu)

(define (si-word pre-zei-bu)
  pre-zei-bu)

(define (su-word any-word-SA-handling)
  any-word-SA-handling)


(define (BRIVLA-clause BRIVLA-pre BRIVLA-post)
  `(BRIVLA-clause ,BRIVLA-pre ,@BRIVLA-post))

(define (BRIVLA-clause-zei zei-clause)
  `((BRIVLA-clause ,@zei-clause)))

(define (BRIVLA-pre pre-clause BRIVLA)
  `(,@pre-clause ,@BRIVLA))

(define (BRIVLA-post post-clause)
  post-clause)


(define (CMENE-clause CMENE-pre CMENE-post)
  `(CMENE-clause ,@CMENE-pre ,@CMENE-post))

(define (CMENE-pre pre-clause CMENE)
  `(,@pre-clause ,@CMENE))

(define (CMENE-post post-clause)
  post-clause)


(define (CMAVO-clause pre-clause CMAVO)
  `(,@pre-clause ,@CMAVO))


(define (A-clause A-pre A-post)
  `((A-clause ,@A-pre ,@A-post)))

(define (A-pre pre-clause A)
  `(,@pre-clause ,@A))

(define (A-post post-clause)
  post-clause)


(define (BAI-clause BAI-pre BAI-post)
  `(BAI-clause ,@BAI-pre ,@BAI-post))

(define (BAI-pre pre-clause BAI)
  `(,@pre-clause ,@BAI))

(define (BAI-post post-clause)
  post-clause)


(define (BAhE-clause clause+)
  (define (BAhE BAhE-pre BAhE-post)
    `(,@BAhE-pre ,@BAhE-post))

  `((BAhE-clause ,@(map-apply BAhE clause+))))

(define (BE-pre pre-clause BE)
  `(,@pre-clause ,@BE))

(define (BAhE-pre BAhE)
  BAhE)

(define (BAhE-post si-clause?)
  (?-null si-clause?))


(define (BE-clause . rodasumti)
  `(BE-clause ,@rodasumti))

(define (BE-pre pre-clause BE)
  `(,@pre-clause ,@BE))

(define (BE-post post-clause)
  post-clause)


(define (BEI-clause . rodasumti)
  `(BEI-clause ,@rodasumti))

(define (BEI-pre pre-clause BEI)
  `(,@pre-clause ,@BEI))

(define (BEI-post post-clause)
  post-clause)


(define (BEhO-clause . rodasumti)
  `(BEhO-clause ,@rodasumti))

(define (BEhO-pre pre-clause BEhO)
  `(,@pre-clause ,@BEhO))

(define (BEhO-post post-clause)
  post-clause)


(define (BIhE-clause . rodasumti)
  `(BIhE-clause ,@rodasumti))

(define (BIhE-pre pre-clause BIhE)
  `(,@pre-clause ,@BIhE))

(define (BIhE-post post-clause)
  post-clause)


(define (BIhI-clause . rodasumti)
  `(BIhI-clause ,@rodasumti))

(define (BIhI-pre pre-clause BIhI)
  `(,@pre-clause ,@BIhI))

(define (BIhI-post post-clause)
  post-clause)


(define (BO-clause . rodasumti)
  `(BO-clause ,@rodasumti))

(define (BO-pre pre-clause BO)
  `(,@pre-clause ,@BO))

(define (BO-post post-clause)
  post-clause)


(define (BOI-clause . rodasumti)
  `(bOI-clause ,@rodasumti))

(define (BOI-pre pre-clause bOI)
  `(,@pre-clause ,@bOI))

(define (BOI-post post-clause)
  post-clause)


(define (BU-clause pre-clause BU)
  `(,@pre-clause ,@BU))


(define (BY-clause BY-pre BY-post)
  `((BY-clause ,@BY-clause ,@BY-post)))

(define (BY-clause-bu bu-clause)
  `((BY-clause ,@bu-clause)))

(define (BY-pre pre-clause BY)
  `(,@pre-clause ,@BY))

(define (BY-post post-clause)
  post-clause)


(define (CAhA-clause . rodasumti)
  `(CAhA-clause ,@rodasumti))

(define (CAhA-pre pre-clause CAhA)
  `(,@pre-clause ,@CAhA))

(define (CAhA-post post-clause)
  post-clause)


(define (CAI-clause . rodasumti)
  `(CAI-clause ,@rodasumti))

(define (CAI-pre pre-clause CAI)
  `(,@pre-clause ,@CAI))

(define (CAI-post post-clause)
  post-clause)


(define (CEI-clause . rodasumti)
  `(CEI-clause ,@rodasumti))

(define (CEI-pre pre-clause CEI)
  `(,@pre-clause ,@CEI))

(define (CEI-post post-clause)
  post-clause)


(define (CEhE-clause . rodasumti)
  `(CEhE-clause ,@rodasumti))

(define (CEhE-pre pre-clause CEhE)
  `(,@pre-clause ,@CEhE))

(define (CEhE-post post-clause)
  post-clause)


(define (CO-clause . rodasumti)
  `(CO-clause ,@rodasumti))

(define (CO-pre pre-clause CO)
  `(,@pre-clause ,@CO))

(define (CO-post post-clause)
  post-clause)


(define (COI-clause COI-pre COI-post)
  `((COI-clause ,@COI-pre ,@COI-post)))

(define (COI-pre pre-clause COI)
  `(,@pre-clause ,@COI))

(define (COI-post post-clause)
  post-clause)


(define (CU-clause CU-pre CU-post)
  `(CU-clause ,CU-pre ,@CU-post))

(define (CU-pre pre-clause CU)
  `(,@pre-clause ,@CU))

(define (CU-post post-clause)
  post-clause)


(define (CUhE-clause . rodasumti)
  `(CUhE-clause ,@rodasumti))

(define (CUhE-pre pre-clause CUhE)
  `(,@pre-clause ,@CUhE))

(define (CUhE-post post-clause)
  post-clause)


(define (DAhO-clause . rodasumti)
  `(DAhO-clause ,@rodasumti))

(define (DAhO-pre pre-clause DAhO)
  `(,@pre-clause ,@DAhO))

(define (DAhO-post post-clause)
  post-clause)


(define (DOI-clause . rodasumti)
  `(DOI-clause ,@rodasumti))

(define (DOI-pre pre-clause DOI)
  `(,@pre-clause ,@DOI))

(define (DOI-post post-clause)
  post-clause)


(define (DOhU-clause . rodasumti)
  `(DOhU-clause ,@rodasumti))

(define (DOhU-pre pre-clause DOhU)
  `(,@pre-clause ,@DOhU))

(define (DOhU-post post-clause)
  post-clause)


(define (FA-clause FA-pre FA-post)
  `(FA-clause ,FA-pre ,@FA-post))

(define (FA-pre pre-clause FA)
  `(,@pre-clause ,@FA))

(define (FA-post post-clause)
  post-clause)


(define (FAhA-clause . rodasumti)
  `(FAhA-clause ,@rodasumti))

(define (FAhA-pre pre-clause FAhA)
  `(,@pre-clause ,@FAhA))

(define (FAhA-post post-clause)
  post-clause)


(define (FAhO-clause pre-clause FAhO)
  `((FAhO-clause ,@pre-clause ,@FAhO)))


(define (FEhE-clause . rodasumti)
  `(FEhE-clause ,@rodasumti))

(define (FEhE-pre pre-clause FEhE)
  `(,@pre-clause ,@FEhE))

(define (FEhE-post post-clause)
  post-clause)


(define (FEhU-clause . rodasumti)
  `(FEhU-clause ,@rodasumti))

(define (FEhU-pre pre-clause FEhU)
  `(,@pre-clause ,@FEhU))

(define (FEhU-post post-clause)
  post-clause)


(define (FIhO-clause . rodasumti)
  `(FIhO-clause ,@rodasumti))

(define (FIhO-pre pre-clause FIhO)
  `(,@pre-clause ,@FIhO))

(define (FIhO-post post-clause)
  post-clause)


(define (FOI-clause . rodasumti)
  `(FOI-clause ,@rodasumti))

(define (FOI-pre pre-clause FOI)
  `(,@pre-clause ,@FOI))

(define (FOI-post post-clause)
  post-clause)


(define (FUhA-clause . rodasumti)
  `(FUhA-clause ,@rodasumti))

(define (FUhA-pre pre-clause FUhA)
  `(,@pre-clause ,@FUhA))

(define (FUhA-post post-clause)
  post-clause)


(define (FUhE-clause . rodasumti)
  `(FUhE-clause ,@rodasumti))

(define (FUhE-pre pre-clause FUhE)
  `(,@pre-clause ,@FUhE))

(define (FUhE-post post-clause)
  post-clause)


(define (FUhO-clause . rodasumti)
  `(FUhO-clause ,@rodasumti))

(define (FUhO-pre pre-clause FUhO)
  `(,@pre-clause ,@FUhO))

(define (FUhO-post post-clause)
  post-clause)


(define (GA-clause . rodasumti)
  `(GA-clause ,@rodasumti))

(define (GA-pre pre-clause GA)
  `(,@pre-clause ,@GA))

(define (GA-post post-clause)
  post-clause)


(define (GAhO-clause . rodasumti)
  `(GAhO-clause ,@rodasumti))

(define (GAhO-pre pre-clause GAhO)
  `(,@pre-clause ,@GAhO))

(define (GAhO-post post-clause)
  post-clause)


(define (GEhU-clause . rodasumti)
  `(GEhU-clause ,@rodasumti))

(define (GEhU-pre pre-clause GEhU)
  `(,@pre-clause ,@GEhU))

(define (GEhU-post post-clause)
  post-clause)


(define (GI-clause . rodasumti)
  `(GI-clause ,@rodasumti))

(define (GI-pre pre-clause GI)
  `(,@pre-clause ,@GI))

(define (GI-post post-clause)
  post-clause)


(define (GIhA-clause . rodasumti)
  `(GIhA-clause ,@rodasumti))

(define (GIhA-pre pre-clause GIhA)
  `(,@pre-clause ,@GIhA))

(define (GIhA-post post-clause)
  post-clause)


(define (GOI-clause GOI-pre GOI-post)
  `(GOI-clause ,@GOI-pre ,@GOI-post))

(define (GOI-pre pre-clause GOI)
  `(,@pre-clause ,GOI))

(define (GOI-post post-clause)
  post-clause)


(define (GOhA-clause GOhA-pre GOhA-post)
  `(GOhA-clause ,@GOhA-pre ,@GOhA-post))

(define (GOhA-pre pre-clause GOhA)
  `(,@pre-clause ,GOhA))

(define (GOhA-post post-clause)
  post-clause)


(define (GUhA-clause . rodasumti)
  `(GUhA-clause ,@rodasumti))

(define (GUhA-pre pre-clause GUhA)
  `(,@pre-clause ,@GUhA))

(define (GUhA-post post-clause)
  post-clause)


(define (I-clause sentence-sa* I-pre I-post)
  `(I-clause ,@(*-null sentence-sa*) ,@I-pre ,@I-post))

(define (I-pre pre-clause I)
  `(,@pre-clause ,I))

(define (I-post post-clause)
  post-clause)


(define (JA-clause . rodasumti)
  `(JA-clause ,@rodasumti))

(define (JA-pre pre-clause JA)
  `(,@pre-clause ,@JA))

(define (JA-post post-clause)
  post-clause)


(define (JAI-clause . rodasumti)
  `(JAI-clause ,@rodasumti))

(define (JAI-pre pre-clause JAI)
  `(,@pre-clause ,@JAI))

(define (JAI-post post-clause)
  post-clause)


(define (JOhI-clause . rodasumti)
  `(JOhI-clause ,@rodasumti))

(define (JOhI-pre pre-clause JOhI)
  `(,@pre-clause ,@JOhI))

(define (JOhI-post post-clause)
  post-clause)


(define (JOI-clause . rodasumti)
  `(JOI-clause ,@rodasumti))

(define (JOI-pre pre-clause JOI)
  `(,@pre-clause ,@JOI))

(define (JOI-post post-clause)
  post-clause)


(define (KE-clause . rodasumti)
  `(KE-clause ,@rodasumti))

(define (KE-pre pre-clause KE)
  `(,@pre-clause ,@KE))

(define (KE-post post-clause)
  post-clause)


(define (KEhE-clause . rodasumti)
  `(KEhE-clause ,@rodasumti))

(define (KEhE-pre pre-clause KEhE)
  `(,@pre-clause ,@KEhE))

(define (KEhE-post post-clause)
  post-clause)


(define (KEI-clause . rodasumti)
  `(KEI-clause ,@rodasumti))

(define (KEI-pre pre-clause KEI)
  `(,@pre-clause ,@KEI))

(define (KEI-post post-clause)
  post-clause)


(define (KI-clause . rodasumti)
  `(KI-clause ,@rodasumti))

(define (KI-pre pre-clause KI)
  `(,@pre-clause ,@KI))

(define (KI-post post-clause)
  post-clause)


(define (KOhA-clause KOhA-pre KOhA-post)
  `(KOhA-clause ,KOhA-pre ,@KOhA-post))

(define (KOhA-pre pre-clause KOhA)
  `(,@pre-clause ,@KOhA))

(define (KOhA-post post-clause)
  post-clause)


(define (KU-clause KU-pre KU-post)
  `(KU-clause ,@KU-pre ,@KU-post))

(define (KU-pre pre-clause KU)
  `(,@pre-clause ,KU))

(define (KU-post post-clause)
  post-clause)


(define (KUhE-clause . rodasumti)
  `(KUhE-clause ,@rodasumti))

(define (KUhE-pre pre-clause KUhE)
  `(,@pre-clause ,@KUhE))

(define (KUhE-post post-clause)
  post-clause)


(define (KUhO-clause . rodasumti)
  `(KUhO-clause ,@rodasumti))

(define (KUhO-pre pre-clause KUhO)
  `(,@pre-clause ,@KUhO))

(define (KUhO-post post-clause)
  post-clause)


(define (LA-clause LA-pre LA-post)
  `(LA-clause ,LA-pre ,@LA-post))

(define (LA-pre pre-clause LA)
  `(,@pre-clause ,@LA))

(define (LA-post post-clause)
  post-clause)


(define (LAU-clause . rodasumti)
  `(LAU-clause ,@rodasumti))

(define (LAU-pre pre-clause LAU)
  `(,@pre-clause ,@LAU))

(define (LAU-post post-clause)
  post-clause)


(define (LAhE-clause LAhE-pre LAhE-post)
  `(LAhE-clause ,@LAhE-pre ,@LAhE-post))

(define (LAhE-pre pre-clause LAhE)
  `(,@pre-clause ,LAhE))

(define (LAhE-post post-clause)
  post-clause)


(define (LE-clause LE-pre LE-post)
  `(LE-clause ,LE-pre ,@LE-post))

(define (LE-pre pre-clause LE)
  `(,@pre-clause ,@LE))

(define (LE-post post-clause)
  post-clause)


(define (LEhU-clause pre-clause LEhU)
  `(LEhU-clause ,@pre-clause ,LEhU))


(define (LI-clause LI-pre LI-post)
  `(LI-clause ,LI-pre ,@LI-post))

(define (LI-pre pre-clause LI)
  `(,@pre-clause ,@LI))

(define (LI-post post-clause)
  post-clause)


(define (LIhU-clause . rodasumti)
  `(LIhU-clause ,@rodasumti))

(define (LIhU-pre pre-clause LIhU)
  `(,@pre-clause ,@LIhU))

(define (LIhU-post post-clause)
  post-clause)


(define (LOhO-clause . rodasumti)
  `(LOhO-clause ,@rodasumti))

(define (LOhO-pre pre-clause LOhO)
  `(,@pre-clause ,@LOhO))

(define (LOhO-post post-clause)
  post-clause)


(define (LOhU-clause LOhU-pre LOhU-post)
  `(LOhU-clause ,@LOhU-pre ,@LOhU-post))

(define (LOhU-pre pre-clause LOhU any-word* LEhU-clause)
  `(,@pre-clause ,LOhU ,@(*-null any-word*) ,LEhU-clause))

(define (LOhU-post post-clause)
  post-clause)


(define (LU-clause LU-pre LU-post)
  `(LU-clause ,@LU-pre ,@LU-post))

(define (LU-pre pre-clause LU)
  `(,@pre-clause ,LU))

(define (LU-post post-clause)
  post-clause)


(define (LUhU-clause . rodasumti)
  `(LUhU-clause ,@rodasumti))

(define (LUhU-pre pre-clause LUhU)
  `(,@pre-clause ,@LUhU))

(define (LUhU-post post-clause)
  post-clause)


(define (MAhO-clause . rodasumti)
  `(MAhO-clause ,@rodasumti))

(define (MAhO-pre pre-clause MAhO)
  `(,@pre-clause ,@MAhO))

(define (MAhO-post post-clause)
  post-clause)


(define (MAI-clause . rodasumti)
  `(MAI-clause ,@rodasumti))

(define (MAI-pre pre-clause MAI)
  `(,@pre-clause ,@MAI))

(define (MAI-post post-clause)
  post-clause)


(define (ME-clause ME-pre ME-post)
  `(ME-clause ,ME-pre ,@ME-post))

(define (ME-pre pre-clause ME)
  `(,@pre-clause ,@ME))

(define (ME-post post-clause)
  post-clause)


(define (MEhU-clause . rodasumti)
  `(MEhU-clause ,@rodasumti))

(define (MEhU-pre pre-clause MEhU)
  `(,@pre-clause ,@MEhU))

(define (MEhU-post post-clause)
  post-clause)


(define (MOhE-clause . rodasumti)
  `(MOhE-clause ,@rodasumti))

(define (MOhE-pre pre-clause MOhE)
  `(,@pre-clause ,@MOhE))

(define (MOhE-post post-clause)
  post-clause)


(define (MOhI-clause . rodasumti)
  `(MOhI-clause ,@rodasumti))

(define (MOhI-pre pre-clause MOhI)
  `(,@pre-clause ,@MOhI))

(define (MOhI-post post-clause)
  post-clause)


(define (MOI-clause . rodasumti)
  `(MOI-clause ,@rodasumti))

(define (MOI-pre pre-clause MOI)
  `(,@pre-clause ,@MOI))

(define (MOI-post post-clause)
  post-clause)


(define (NA-clause NA-pre NA-post)
  `(NA-clause ,@NA-pre ,@NA-post))

(define (NA-pre pre-clause NA)
  `(,@pre-clause ,NA))

(define (NA-post post-clause)
  post-clause)


(define (NAI-clause NAI-pre NAI-post)
  `(NAI-clause ,@NAI-pre ,@NAI-post))

(define (NAI-pre pre-clause NAI)
  `(,@pre-clause ,NAI))

(define (NAI-post post-clause)
  post-clause)


(define (NAhE-clause NAhE-pre NAhE-post)
  `(NAhE-clause ,@NAhE-pre ,@NAhE-post))

(define (NAhE-pre pre-clause NAhE)
  `(,@pre-clause ,NAhE))

(define (NAhE-post post-clause)
  post-clause)


(define (NAhU-clause . rodasumti)
  `(NAhU-clause ,@rodasumti))

(define (NAhU-pre pre-clause NAhU)
  `(,@pre-clause ,@NAhU))

(define (NAhU-post post-clause)
  post-clause)


(define (NIhE-clause . rodasumti)
  `(NIhE-clause ,@rodasumti))

(define (NIhE-pre pre-clause NIhE)
  `(,@pre-clause ,@NIhE))

(define (NIhE-post post-clause)
  post-clause)


(define (NIhO-clause sentence-sa* NIhO-pre NIhO-post)
  `(NIhO-clause ,@(*-null sentence-sa*) ,@NIhO-pre ,@NIhO-post))

(define (NIhO-pre pre-clause NIhO)
  `(,@pre-clause ,NIhO))

(define (NIhO-post su-clause* post-clause)
  `(,@(*-null su-clause*) ,@post-clause))


(define (NOI-clause . rodasumti)
  `(NOI-clause ,@rodasumti))

(define (NOI-pre pre-clause NOI)
  `(,@pre-clause ,@NOI))

(define (NOI-post post-clause)
  post-clause)


(define (NU-clause . rodasumti)
  `(NU-clause ,@rodasumti))

(define (NU-pre pre-clause NU)
  `(,@pre-clause ,@NU))

(define (NU-post post-clause)
  post-clause)


(define (NUhA-clause . rodasumti)
  `(NUhA-clause ,@rodasumti))

(define (NUhA-pre pre-clause NUhA)
  `(,@pre-clause ,@NUhA))

(define (NUhA-post post-clause)
  post-clause)


(define (NUhI-clause . rodasumti)
  `(NUhI-clause ,@rodasumti))

(define (NUhI-pre pre-clause NUhI)
  `(,@pre-clause ,@NUhI))

(define (NUhI-post post-clause)
  post-clause)


(define (NUhU-clause . rodasumti)
  `(NUhU-clause ,@rodasumti))

(define (NUhU-pre pre-clause NUhU)
  `(,@pre-clause ,@NUhU))

(define (NUhU-post post-clause)
  post-clause)


(define (PA-clause . rodasumti)
  `(PA-clause ,@rodasumti))

(define (PA-pre pre-clause PA)
  `(,@pre-clause ,@PA))

(define (PA-post post-clause)
  post-clause)


(define (PEhE-clause . rodasumti)
  `(PEhE-clause ,@rodasumti))

(define (PEhE-pre pre-clause PEhE)
  `(,@pre-clause ,@PEhE))

(define (PEhE-post post-clause)
  post-clause)


(define (PEhO-clause . rodasumti)
  `(PEhO-clause ,@rodasumti))

(define (PEhO-pre pre-clause PEhO)
  `(,@pre-clause ,@PEhO))

(define (PEhO-post post-clause)
  post-clause)


(define (PU-clause . rodasumti)
  `(PU-clause ,@rodasumti))

(define (PU-pre pre-clause PU)
  `(,@pre-clause ,@PU))

(define (PU-post post-clause)
  post-clause)


(define (RAhO-clause . rodasumti)
  `(RAhO-clause ,@rodasumti))

(define (RAhO-pre pre-clause RAhO)
  `(,@pre-clause ,@RAhO))

(define (RAhO-post post-clause)
  post-clause)


(define (ROI-clause . rodasumti)
  `(ROI-clause ,@rodasumti))

(define (ROI-pre pre-clause ROI)
  `(,@pre-clause ,@ROI))

(define (ROI-post post-clause)
  post-clause)


(define (SA-clause SA-pre)
  `(SA-clause ,SA-pre))


(define (SE-clause SE-pre SE-post)
  `(SE-clause ,@SE-pre ,@SE-post))

(define (SE-pre pre-clause SE)
  `(,@pre-clause ,SE))

(define (SE-post post-clause)
  post-clause)


(define (SEI-clause . rodasumti)
  `(SEI-clause ,@rodasumti))

(define (SEI-pre pre-clause SEI)
  `(,@pre-clause ,@SEI))

(define (SEI-post post-clause)
  post-clause)


(define (SEhU-clause . rodasumti)
  `(SEhU-clause ,@rodasumti))

(define (SEhU-pre pre-clause SEhU)
  `(,@pre-clause ,@SEhU))

(define (SEhU-post post-clause)
  post-clause)


(define (SI-clause SI)
  `((SI-clause ,SI)))

(define (SI-pre pre-clause SI)
  `(,@pre-clause ,@SI))

(define (SI-post post-clause)
  post-clause)


(define (SOI-clause . rodasumti)
  `(SOI-clause ,@rodasumti))

(define (SOI-pre pre-clause SOI)
  `(,@pre-clause ,@SOI))

(define (SOI-post post-clause)
  post-clause)


(define (SU-clause . rodasumti)
  `(SU-clause ,@rodasumti))

(define (SU-pre pre-clause SU)
  `(,@pre-clause ,@SU))

(define (SU-post post-clause)
  post-clause)


(define (TAhE-clause . rodasumti)
  `(TAhE-clause ,@rodasumti))

(define (TAhE-pre pre-clause TAhE)
  `(,@pre-clause ,@TAhE))

(define (TAhE-post post-clause)
  post-clause)


(define (TEhU-clause . rodasumti)
  `(TEhU-clause ,@rodasumti))

(define (TEhU-pre pre-clause TEhU)
  `(,@pre-clause ,@TEhU))

(define (TEhU-post post-clause)
  post-clause)


(define (TEI-clause . rodasumti)
  `(TEI-clause ,@rodasumti))

(define (TEI-pre pre-clause TEI)
  `(,@pre-clause ,@TEI))

(define (TEI-post post-clause)
  post-clause)


(define (TO-clause TO-pre TO-post)
  `(TO-clause ,@TO-pre ,@TO-post))

(define (TO-pre pre-clause TO)
  `(,@pre-clause ,TO))

(define (TO-post post-clause)
  post-clause)


(define (TOI-clause TOI-pre TOI-post)
  `(TOI-clause ,@TOI-pre ,@TOI-post))

(define (TOI-pre pre-clause TOI)
  `(,@pre-clause ,TOI))

(define (TOI-post post-clause)
  post-clause)


(define (TUhE-clause . rodasumti)
  `(TUhE-clause ,@rodasumti))

(define (TUhE-pre pre-clause TUhE)
  `(,@pre-clause ,@TUhE))

(define (TUhE-post post-clause)
  post-clause)


(define (TUhU-clause . rodasumti)
  `(TUhU-clause ,@rodasumti))

(define (TUhU-pre pre-clause TUhU)
  `(,@pre-clause ,@TUhU))

(define (TUhU-post post-clause)
  post-clause)


(define (UI-clause UI-pre UI-post)
  `(UI-clause ,@UI-pre ,@UI-post))

(define (UI-pre pre-clause UI)
  `(,@pre-clause ,UI))

(define (UI-post post-clause)
  post-clause)


(define (VA-clause . rodasumti)
  `(VA-clause ,@rodasumti))

(define (VA-pre pre-clause VA)
  `(,@pre-clause ,@VA))

(define (VA-post post-clause)
  post-clause)


(define (VAU-clause . rodasumti)
  `(VAU-clause ,@rodasumti))

(define (VAU-pre pre-clause VAU)
  `(,@pre-clause ,@VAU))

(define (VAU-post post-clause)
  post-clause)


(define (VEI-clause . rodasumti)
  `(VEI-clause ,@rodasumti))

(define (VEI-pre pre-clause VEI)
  `(,@pre-clause ,@VEI))

(define (VEI-post post-clause)
  post-clause)


(define (VEhO-clause . rodasumti)
  `(VEhO-clause ,@rodasumti))

(define (VEhO-pre pre-clause VEhO)
  `(,@pre-clause ,@VEhO))

(define (VEhO-post post-clause)
  post-clause)


(define (VUhU-clause . rodasumti)
  `(VUhU-clause ,@rodasumti))

(define (VUhU-pre pre-clause VUhU)
  `(,@pre-clause ,@VUhU))

(define (VUhU-post post-clause)
  post-clause)


(define (VEhA-clause . rodasumti)
  `(VEhA-clause ,@rodasumti))

(define (VEhA-pre pre-clause VEhA)
  `(,@pre-clause ,@VEhA))

(define (VEhA-post post-clause)
  post-clause)


(define (VIhA-clause . rodasumti)
  `(VIhA-clause ,@rodasumti))

(define (VIhA-pre pre-clause VIhA)
  `(,@pre-clause ,@VIhA))

(define (VIhA-post post-clause)
  post-clause)


(define (VUhO-clause . rodasumti)
  `(VUhO-clause ,@rodasumti))

(define (VUhO-pre pre-clause VUhO)
  `(,@pre-clause ,@VUhO))

(define (VUhO-post post-clause)
  post-clause)


(define (XI-clause . rodasumti)
  `(XI-clause ,@rodasumti))

(define (XI-pre pre-clause XI)
  `(,@pre-clause ,@XI))

(define (XI-post post-clause)
  post-clause)


;(define (Y-clause Y)
;  `((Y-clause ,@Y)))


(define (ZAhO-clause ZAhO-pre ZAhO-post)
  `(ZAhO-clause ,@ZAhO-pre ZAhO-post))

(define (ZAhO-pre pre-clause ZAhO)
  `(,@pre-clause ,@ZAhO))

(define (ZAhO-post post-clause)
  post-clause)


(define (ZEhA-clause ZEhA-pre ZEhA-post)
  `(ZEhA-clause ,@ZEhA-pre ,@ZEhA-post))

(define (ZEhA-pre pre-clause ZEhA)
  `(,@pre-clause ,@ZEhA))

(define (ZEhA-post post-clause)
  post-clause)


(define (ZEI-clause ZEI-pre ZEI-post)
  `(ZEI-clause ,@ZEI-pre ,@ZEI-post))

(define (ZEI-pre pre-clause ZEI)
  `(,@pre-clause ,@ZEI))

(define (ZEI-post post-clause)
  post-clause)


(define (ZI-clause ZI-pre ZI-post)
  `(ZI-clause ,@ZI-pre ,@ZI-post))

(define (ZI-pre pre-clause ZI)
  `(,@pre-clause ,@ZI))

(define (ZI-post post-clause)
  post-clause)


(define (ZIhE-clause ZIhE-pre ZIhE-post)
  `(ZIhE-clause ,@ZIhE-pre ,@ZIhE-post))

(define (ZIhE-pre pre-clause ZIhE)
  `(,@pre-clause ,@ZIhE))

(define (ZIhE-post post-clause)
  post-clause)


(define (ZO-clause ZO-pre ZO-post)
  `(ZO-clause ,@ZO-pre ,@ZO-post))

(define (ZO-pre pre-clause ZO any-word)
  `(,@pre-clause ,ZO ,any-word))

(define (ZO-post post-clause)
  post-clause)


(define (ZOI-clause ZOI-pre ZOI-post)
  `(ZOI-clause ,@ZOI-pre ,@ZOI-post))

(define (ZOI-pre pre-clause ZOI zoi-open zoi-word* zoi-close)
  `(,@pre-clause ,ZOI ,zoi-open ,@(*-null zoi-word*) ,zoi-close))

(define (ZOI-post post-clause)
  post-clause)


(define (ZOhU-clause ZOhU-pre ZOhU-post)
  `(ZOhU-clause ,@ZOhU-pre ,@ZOhU-post))

(define (ZOhU-pre pre-clause ZOhU)
  `(,@pre-clause ,@ZOhU))

(define (ZOhU-post post-clause)
  post-clause)


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
