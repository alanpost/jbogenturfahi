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

(module jbogensamselpla
  (a
   e
   i
   o
   u
   y
   l
   m
   n
   r
   b
   d
   g
   v
   j
   z
   s
   c
   x
   k
   f
   p
   t
   h
   digit
   final-syllable
   any-syllable
   syllable
   consonantal-syllable
   coda
   onset
   diphthong
   cluster
   initial-pair
   initial
   affricate
   zifcme
   jbocme
   CVCy-lujvo-brivla-core
   CVCy-lujvo-short-final-rafsi
   cmavo-form
   cmavo-form-y
   lujvo
   fuhivla
   stressed-brivla-rafsi
   brivla-rafsi
   stressed-fuhivla-rafsi
   fuhivla-rafsi
   brivla-head
   slinkuhi
   rafsi-string
   rafsi-string-short-final
   rafsi-string-initial-pair
   gismu
   CVV-final-rafsi
   short-final-rafsi
   stressed-y-rafsi
   stressed-long-rafsi-CCVC
   stressed-long-rafsi-CVCC
   stressed-CVC-rafsi
   stressed-CCV-rafsi
   stressed-CVV-rafsi
   stressed-CVV-rafsi-VhV
   y-rafsi
   long-rafsi-CCVC
   long-rafsi-CVCC
   CVC-rafsi
   CCV-rafsi
   CVV-rafsi
   CVV-rafsi-VhV
   non-lojban-word
   ybu
   cmavo
   A
   BAI
   BAhE
   BE
   BEI
   BEhO
   BIhE
   BIhI
   BO
   BOI
   BU
   BY
   CAhA
   CAI
   CEI
   CEhE
   CO
   COI
   CU
   CUhE
   DAhO
   DOI
   DOhU
   FA
   FAhA
   FAhO
   FEhE
   FEhU
   FIhO
   FOI
   FUhA
   FUhE
   FUhO
   GA
   GAhO
   GEhU
   GI
   GIhA
   GOI
   GOhA
   GUhA
   I
   JA
   JAI
   JOhI
   JOI
   KE
   KEhE
   KEI
   KI
   KOhA
   KU
   KUhE
   KUhO
   LA
   LAU
   LAhE
   LE
   LEhU
   LI
   LIhU
   LOhO
   LOhU
   LU
   LUhU
   MAhO
   MAI
   ME
   MEhU
   MOhE
   MOhI
   MOI
   NA
   NAI
   NAhE
   NAhU
   NIhE
   NIhO
   NOI
   NU
   NUhA
   NUhI
   NUhU
   PA
   PEhE
   PEhO
   PU
   RAhO
   ROI
   SA
   SE
   SEI
   SEhU
   SI
   SOI
   SU
   TAhE
   TEhU
   TEI
   TO
   TOI
   TUhE
   TUhU
   UI
   VA
   VAU
   VEI
   VEhO
   VEhA
   VIhA
   VUhO
   VUhU
   XI
   Y
   ZAhO
   ZEhA
   ZEI
   ZI
   ZIhE
   ZO
   ZOI
   ZOhU
   
   text
   paragraphs
   paragraph
   statement

   sumti
   sumti-BOI
   sumti-LU
   sumti-LAhE
   sumti-KOhA
   sumti-LA
   sumti-LE

   li-clause

   selbri
   selbri-NA

   tanru-unit

   tanru-unit-GOhA
   tanru-unit-KE
   tanru-unit-ME
   tanru-unit-MOI
   tanru-unit-NUhA
   tanru-unit-SE
   tanru-unit-JAI
   tanru-unit-NAhE
   tanru-unit-NU

   linkargs
   linkargs-1
   linkargs-sa

   links
   links-1
   links-sa

   quantifier-BOI
   quantifier-VEI

   mex

   operand-BOI

   number
   lerfu-string

   free-SEI
   free-SOI
   free-vocative-selbri
   free-vocative-cmene
   free-vocative-sumti
   free-MAI
   free-TO

   xi-clause-BOI
   xi-clause-VEI
   
   vocative
   indicators
   indicator

   zei-clause
   bu-clause

   pre-clause
   post-clause

   A-clause
   BAI-clause
   BAhE-clause
   BE-clause
   BEI-clause
   BEhO-clause
   BIhE-clause
   BIhI-clause
   BO-clause
   bOI-clause
   BU-clause
   BY-clause
   CAhA-clause
   CAI-clause
   CEI-clause
   CEhE-clause
   CO-clause
   COI-clause
   CU-clause
   CUhE-clause
   DAhO-clause
   DOI-clause
   DOhU-clause
   FA-clause
   FAhA-clause
   FAhO-clause
   FEhE-clause
   FEhU-clause
   FIhO-clause
   FOI-clause
   FUhA-clause
   FUhE-clause
   FUhO-clause
   GA-clause
   GAhO-clause
   GEhU-clause
   GI-clause
   GIhA-clause
   GOI-clause
   GOhA-clause
   GUhA-clause
   I-clause
   JA-clause
   JAI-clause
   JOhI-clause
   JOI-clause
   KE-clause
   KEhE-clause
   KEI-clause
   KI-clause
   KOhA-clause
   KU-clause
   KUhE-clause
   KUhO-clause
   LA-clause
   LAU-clause
   LAhE-clause
   LE-clause
   LEhU-clause
   LI-clause
   LIhU-clause
   LOhO-clause
   LOhU-clause
   LU-clause
   LUhU-clause
   MAhO-clause
   MAI-clause
   ME-clause
   MEhU-clause
   MOhE-clause
   MOhI-clause
   MOI-clause
   NA-clause
   NAI-clause
   NAhE-clause
   NAhU-clause
   NIhE-clause
   NIhO-clause
   NOI-clause
   NU-clause
   NUhA-clause
   NUhI-clause
   NUhU-clause
   PA-clause
   PEhE-clause
   PEhO-clause
   PU-clause
   RAhO-clause
   ROI-clause
   SA-clause
   SE-clause
   SEI-clause
   SEhU-clause
   SI-clause
   SOI-clause
   SU-clause
   TAhE-clause
   TEhU-clause
   TEI-clause
   TO-clause
   TOI-clause
   TUhE-clause
   TUhU-clause
   UI-clause
   VA-clause
   VAU-clause
   VEI-clause
   VEhO-clause
   VUhU-clause
   VEhA-clause
   VIhA-clause
   VUhO-clause
   XI-clause
   ;Y-clause
   ZAhO-clause
   ZEhA-clause
   ZEI-clause
   ZI-clause
   ZIhE-clause
   ZO-clause
   ZOI-clause
   ZOhU-clause

   zoi-open
   zoi-word
   zoi-close)

(import chicken)
(import scheme)

(require-library genturfahi)

(import genturfahi)

(include "c0re.scm")
(include "samselpla.scm"))
