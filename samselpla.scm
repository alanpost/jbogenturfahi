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
(define (affricate stop fricative)
  (string-append stop fricative))

(define (any-syllable onset nucleus coda)
  (string-append onset nucleus coda))

(define (cluster cfari fanmo)
  (apply string-append cfari fanmo))

(define (cmavo-form onset nucleus nucleus-fanmo)
  (string onset (apply string-append nucleus) nucleus-fanmo))

(define (cmavo-form-y y)
  (apply string-append y))

(define (coda syllabic consonant)
  (string-append syllabic consonant))

(define (consonantal-syllable consonant0 syllabic consonant1)
  (string-append consonant1 syllabic consonant1))

(define (diphthong vowel0 vowel1)
  (string-append vowel0 vowel1))

(define (initial sibilant other liquid)
  (string-append sibilant other liquid))

(define (initial-pair consonant0 consonant1)
  (string-append consonant0 consonant1))

(define (onset consonant glide)
  (string-append consonant glide))

;;
;; selma'o
;;

(define (A rodalerfu)
  (apply string-append rodalerfu))

(define (BAI rodalerfu)
  (apply string-append rodalerfu))

(define (BAhE rodalerfu)
  (apply string-append rodalerfu))

(define (BE rodalerfu)
  (apply string-append rodalerfu))

(define (BEI rodalerfu)
  (apply string-append rodalerfu))

(define (BEhO rodalerfu)
  (apply string-append rodalerfu))

(define (BIhE rodalerfu)
  (apply string-append rodalerfu))

(define (BIhI rodalerfu)
  (apply string-append rodalerfu))

(define (BO rodalerfu)
  (apply string-append rodalerfu))

(define (BOI rodalerfu)
  (apply string-append rodalerfu))

(define (BU rodalerfu)
  (apply string-append rodalerfu))

(define (BY rodalerfu)
  (apply string-append rodalerfu))

(define (CAhA rodalerfu)
  (apply string-append rodalerfu))

(define (CAI rodalerfu)
  (apply string-append rodalerfu))

(define (CEI rodalerfu)
  (apply string-append rodalerfu))

(define (CEhE rodalerfu)
  (apply string-append rodalerfu))

(define (CO rodalerfu)
  (apply string-append rodalerfu))

(define (COI rodalerfu)
  (apply string-append rodalerfu))

(define (CU rodalerfu)
  (apply string-append rodalerfu))

(define (CUhE rodalerfu)
  (apply string-append rodalerfu))

(define (DAhO rodalerfu)
  (apply string-append rodalerfu))

(define (DOI rodalerfu)
  (apply string-append rodalerfu))

(define (DOhU rodalerfu)
  (apply string-append rodalerfu))

(define (FA rodalerfu)
  (apply string-append rodalerfu))

(define (FAhA rodalerfu)
  (apply string-append rodalerfu))

(define (FAhO rodalerfu)
  (apply string-append rodalerfu))

(define (FEhE rodalerfu)
  (apply string-append rodalerfu))

(define (FEhU rodalerfu)
  (apply string-append rodalerfu))

(define (FIhO rodalerfu)
  (apply string-append rodalerfu))

(define (FOI rodalerfu)
  (apply string-append rodalerfu))

(define (FUhA rodalerfu)
  (apply string-append rodalerfu))

(define (FUhE rodalerfu)
  (apply string-append rodalerfu))

(define (FUhO rodalerfu)
  (apply string-append rodalerfu))

(define (GA rodalerfu)
  (apply string-append rodalerfu))

(define (GAhO rodalerfu)
  (apply string-append rodalerfu))

(define (GEhU rodalerfu)
  (apply string-append rodalerfu))

(define (GI rodalerfu)
  (apply string-append rodalerfu))

(define (GIhA rodalerfu)
  (apply string-append rodalerfu))

(define (GOI rodalerfu)
  (apply string-append rodalerfu))

(define (GOhA rodalerfu)
  (apply string-append rodalerfu))

(define (GUhA rodalerfu)
  (apply string-append rodalerfu))

(define (I rodalerfu)
  (apply string-append rodalerfu))

(define (JA rodalerfu)
  (apply string-append rodalerfu))

(define (JAI rodalerfu)
  (apply string-append rodalerfu))

(define (JOhI rodalerfu)
  (apply string-append rodalerfu))

(define (JOI rodalerfu)
  (apply string-append rodalerfu))

(define (KE rodalerfu)
  (apply string-append rodalerfu))

(define (KEhE rodalerfu)
  (apply string-append rodalerfu))

(define (KEI rodalerfu)
  (apply string-append rodalerfu))

(define (KI rodalerfu)
  (apply string-append rodalerfu))

(define (KOhA rodalerfu)
  (apply string-append rodalerfu))

(define (KU rodalerfu)
  (apply string-append rodalerfu))

(define (KUhE rodalerfu)
  (apply string-append rodalerfu))

(define (KUhO rodalerfu)
  (apply string-append rodalerfu))

(define (LA rodalerfu)
  (apply string-append rodalerfu))

(define (LAU rodalerfu)
  (apply string-append rodalerfu))

(define (LAhE rodalerfu)
  (apply string-append rodalerfu))

(define (LE rodalerfu)
  (apply string-append rodalerfu))

(define (LEhU rodalerfu)
  (apply string-append rodalerfu))

(define (LI rodalerfu)
  (apply string-append rodalerfu))

(define (LIhU rodalerfu)
  (apply string-append rodalerfu))

(define (LOhO rodalerfu)
  (apply string-append rodalerfu))

(define (LOhU rodalerfu)
  (apply string-append rodalerfu))

(define (LU rodalerfu)
  (apply string-append rodalerfu))

(define (LUhU rodalerfu)
  (apply string-append rodalerfu))

(define (MAhO rodalerfu)
  (apply string-append rodalerfu))

(define (MAI rodalerfu)
  (apply string-append rodalerfu))

(define (ME rodalerfu)
  (apply string-append rodalerfu))

(define (MEhU rodalerfu)
  (apply string-append rodalerfu))

(define (MOhE rodalerfu)
  (apply string-append rodalerfu))

(define (MOhI rodalerfu)
  (apply string-append rodalerfu))

(define (MOI rodalerfu)
  (apply string-append rodalerfu))

(define (NA rodalerfu)
  (apply string-append rodalerfu))

(define (NAI rodalerfu)
  (apply string-append rodalerfu))

(define (NAhE rodalerfu)
  (apply string-append rodalerfu))

(define (NAhU rodalerfu)
  (apply string-append rodalerfu))

(define (NIhE rodalerfu)
  (apply string-append rodalerfu))

(define (NIhO rodalerfu)
  (apply string-append rodalerfu))

(define (NOI rodalerfu)
  (apply string-append rodalerfu))

(define (NU rodalerfu)
  (apply string-append rodalerfu))

(define (NUhA rodalerfu)
  (apply string-append rodalerfu))

(define (NUhI rodalerfu)
  (apply string-append rodalerfu))

(define (NUhU rodalerfu)
  (apply string-append rodalerfu))

(define (PA rodalerfu)
  (apply string-append rodalerfu))

(define (PEhE rodalerfu)
  (apply string-append rodalerfu))

(define (PEhO rodalerfu)
  (apply string-append rodalerfu))

(define (PU rodalerfu)
  (apply string-append rodalerfu))

(define (RAhO rodalerfu)
  (apply string-append rodalerfu))

(define (ROI rodalerfu)
  (apply string-append rodalerfu))

(define (SA rodalerfu)
  (apply string-append rodalerfu))

(define (SE rodalerfu)
  (apply string-append rodalerfu))

(define (SEI rodalerfu)
  (apply string-append rodalerfu))

(define (SEhU rodalerfu)
  (apply string-append rodalerfu))

(define (SI rodalerfu)
  (apply string-append rodalerfu))

(define (SOI rodalerfu)
  (apply string-append rodalerfu))

(define (SU rodalerfu)
  (apply string-append rodalerfu))

(define (TAhE rodalerfu)
  (apply string-append rodalerfu))

(define (TEhU rodalerfu)
  (apply string-append rodalerfu))

(define (TEI rodalerfu)
  (apply string-append rodalerfu))

(define (TO rodalerfu)
  (apply string-append rodalerfu))

(define (TOI rodalerfu)
  (apply string-append rodalerfu))

(define (TUhE rodalerfu)
  (apply string-append rodalerfu))

(define (TUhU rodalerfu)
  (apply string-append rodalerfu))

(define (UI rodalerfu)
  (apply string-append rodalerfu))

(define (VA rodalerfu)
  (apply string-append rodalerfu))

(define (VAU rodalerfu)
  (apply string-append rodalerfu))

(define (VEI rodalerfu)
  (apply string-append rodalerfu))

(define (VEhO rodalerfu)
  (apply string-append rodalerfu))

(define (VEhA rodalerfu)
  (apply string-append rodalerfu))

(define (VIhA rodalerfu)
  (apply string-append rodalerfu))

(define (VUhO rodalerfu)
  (apply string-append rodalerfu))

(define (VUhU rodalerfu)
  (apply string-append rodalerfu))

(define (XI rodalerfu)
  (apply string-append rodalerfu))

(define (Y rodalerfu)
  (apply string-append rodalerfu))

(define (ZAhO rodalerfu)
  (apply string-append rodalerfu))

(define (ZEhA rodalerfu)
  (apply string-append rodalerfu))

(define (ZEI rodalerfu)
  (apply string-append rodalerfu))

(define (ZI rodalerfu)
  (apply string-append rodalerfu))

(define (ZIhE rodalerfu)
  (apply string-append rodalerfu))

(define (ZO rodalerfu)
  (apply string-append rodalerfu))

(define (ZOI rodalerfu)
  (apply string-append rodalerfu))

(define (ZOhU rodalerfu)
  (apply string-append rodalerfu))

;; cmene
;;
(define (zifcme nafanmo consonant)
  (string (apply string-append nafanmo) consonant))

(define (jbocme any-syllable-or-digit)
  (apply string-append any-syllable-or-digit))

;; gismu
;;
(define (gismu initial consonant vowel)
  (string-append (apply string-append initial) consonant vowel))

;; non-Lojban word
;;
(define (non-lojban-word rodalerfu)
  (apply string rodalerfu))
