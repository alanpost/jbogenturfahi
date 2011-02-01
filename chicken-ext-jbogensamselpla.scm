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

   intro-null
   text-part-2
   intro-si-clause
   faho-clause

   text-1-I
   text-1-NIhO
   text-1-paragraphs

   paragraphs
   paragraph
   statement
   statement-1
   statement-2
   statement-3
   statement-3-TUhE

   fragment-prenex
   fragment-terms
   fragment-ek
   fragment-gihek
   fragment-quantifier
   fragment-NA
   fragment-relative-clauses
   fragment-links
   fragment-linkargs

   prenex
   sentence
   sentence-sa
   sentence-start-I
   sentence-start-NIhO
   subsentence
   subsentence-prenex

   bridi-tail

   bridi-tail-sa

   bridi-tail-start-ME
   bridi-tail-start-NUhA
   bridi-tail-start-NU
   bridi-tail-start-NA
   bridi-tail-start-NAhE

   bridi-tail-start-selbri
   bridi-tail-start-tag
   bridi-tail-start-KE
   bridi-tail-start

   bridi-tail-1

   bridi-tail-2

   bridi-tail-3-selbri
   bridi-tail-3-gek

   gek-sentence
   gek-sentence-KE
   gek-sentence-NA

   tail-terms

   terms
   terms-1
   terms-2

   pehe-sa
   cehe-sa

   term

   term-1-sumti
   term-1-FA
   term-1-FA-tag
   term-1-FA-clause
   term-1-FA-sumti
   term-1-FA-KU
   term-1-termset
   term-1-NA

   term-sa

   term-start
   term-start-LA
   term-start-LE
   term-start-LI
   term-start-LU
   term-start-LAhE
   term-start-quantifier
   term-start-gek
   term-start-FA
   term-start-tag

   termset
   termset-gek


   terms-gik-terms
   gek-termset
   termset-terms

   sumti
   sumti-1
   sumti-2
   sumti-3
   sumti-4
   sumti-4-gek
   sumti-5
   sumti-5-selbri
   sumti-6-ZO
   sumti-6-ZOI
   sumti-6-LOhU
   sumti-6-BOI
   sumti-6-LU
   sumti-6-LAhE-clause
   sumti-6-LAhE-NAhE
   sumti-6-LAhE
   sumti-6-KOhA
   sumti-6-LA
   sumti-6-LE
   sumti-6-LI
   sumti-tail-sumti-6
   sumti-tail
   sumti-tail-1-selbri
   sumti-tail-1-sumti

   relative-clauses
   relative-clause
   relative-clause-sa
   relative-clause-1-GOI
   relative-clause-1-NOI
   relative-clause-start

   selbri
   selbri-1
   selbri-1-NA
   selbri-2
   selbri-3
   selbri-4
   selbri-4-joik-jek
   selbri-4-joik
   selbri-5
   selbri-6
   selbri-6-NAhE

   tanru-unit

   tanru-unit-1

   tanru-unit-2-BRIVLA
   tanru-unit-2-GOhA
   tanru-unit-2-KE
   tanru-unit-2-ME
   tanru-unit-2-MOI
   tanru-unit-2-NUhA
   tanru-unit-2-SE
   tanru-unit-2-JAI
   tanru-unit-2-NAhE
   tanru-unit-2-NU

   linkargs
   linkargs-1
   linkargs-sa

   linkargs-start

   links
   links-sa
   links-1

   links-start

   quantifier-BOI
   quantifier-VEI

   mex
   mex-sa
   mex-0
   mex-0-rp
   mex-start-FUhA
   mex-start-PEhO
   mex-start
   rp-clause
   mex-1
   mex-2
   mex-forethought
   fore-operands
   rp-expression
   rp-expression-tail
   operator
   operator-0
   operator-0-joik-jek
   operator-0-joik
   operator-sa
   operator-start
   operator-start-KE
   operator-start-NAhE
   operator-start-MAhO
   operator-start-VUhU
   operator-1
   operator-gukek
   operator-jek
   operator-2
   operator-2-KE
   mex-operator
   mex-operator-NAhE
   mex-operator-MAhO
   mex-operator-NAhU
   mex-operator-VUhU
   operand
   operand-sa
   operand-0
   operand-start-quantifier
   operand-start-lerfu-word
   operand-start-NIhE
   operand-start-MOhE
   operand-start-JOhI
   operand-start-gek
   operand-start-LAhE
   operand-start-NAhE
   operand-1
   operand-2
   operand-3
   operand-3-BOI
   operand-3-NIhE
   operand-3-MOhE
   operand-3-JOhI
   operand-3-gek
   operand-3-LAhE
   operand-3-NAhE
   operand-BOI

   number
   lerfu-string

   lerfu-word-BY
   lerfu-word-LAU
   lerfu-word-TEI

   ek

   gihek
   gihek-sa
   gihek-1

   jek

   joik-JOI
   joik-interval
   joik-GAhO

   interval

   joik-ek
   joik-ek-sa
   joik-ek-1-joik
   joik-ek-1-ek

   joik-jek

   gek-GA
   gek-GI
   gek-gik

   guhek
   gik

   tag

   stag-tense
   stag-simple-tense

   tense-modal-simple-tense
   tense-modal-FIhO

   simple-tense-modal-BAI
   simple-tense-modal-CUhE
   simple-tense-modal-KI
   simple-tense-modal-CAhA

   time-ZI
   time-offset

   space
   space-offset

   space-interval
   space-interval-VEhA

   space-int-props

   interval-property
   interval-property-TAhE
   interval-property-ZAhO


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
   zei-clause-no-pre

   bu-clause
   bu-clause-no-pre

   zei-tail
   bu-tail

   pre-zei-bu

   pre-clause
   post-clause

   su-clause
   si-clause

   erasable-clause-zei
   erasable-clause-bu

   sa-word
   si-word
   su-word

   BRIVLA-clause
   BRIVLA-clause-zei
   BRIVLA-pre
   BRIVLA-post

   CMENE-clause
   CMENE-pre
   CMENE-post

   CMAVO-clause

   A-clause
   A-pre
   A-post

   BAI-clause
   BAI-pre
   BAI-post

   BAhE-clause
   BAhE-pre
   BAhE-post

   BE-clause
   BE-pre
   BE-post

   BEI-clause
   BEI-pre
   BEI-post

   BEhO-clause
   BEhO-pre
   BEhO-post

   BIhE-clause
   BIhE-pre
   BIhE-post

   BIhI-clause
   BIhI-pre
   BIhI-post

   BO-clause
   BO-pre
   BO-post

   BOI-clause
   BOI-pre
   BOI-post

   BU-clause

   BY-clause
   BY-pre
   BY-post

   BY-clause
   BY-clause-bu
   BY-pre
   BY-post

   CAhA-clause
   CAhA-pre
   CAhA-post

   CAI-clause
   CAI-pre
   CAI-post

   CEI-clause
   CEI-pre
   CEI-post

   CEhE-clause
   CEhE-pre
   CEhE-post

   CO-clause
   CO-pre
   CO-post

   COI-clause
   COI-pre
   COI-post

   CU-clause
   CU-pre
   CU-post

   CUhE-clause
   CUhE-pre
   CUhE-post

   DAhO-clause
   DAhO-pre
   DAhO-post

   DOI-clause
   DOI-pre
   DOI-post

   DOhU-clause
   DOhU-pre
   DOhU-post

   FA-clause
   FA-pre
   FA-post

   FAhA-clause
   FAhA-pre
   FAhA-post

   FAhO-clause

   FEhE-clause
   FEhE-pre
   FEhE-post

   FEhU-clause
   FEhU-pre
   FEhU-post

   FIhO-clause
   FIhO-pre
   FIhO-post

   FOI-clause
   FOI-pre
   FOI-post

   FUhA-clause
   FUhA-pre
   FUhA-post

   FUhE-clause
   FUhE-pre
   FUhE-post

   FUhO-clause
   FUhO-pre
   FUhO-post

   GA-clause
   GA-pre
   GA-post

   GAhO-clause
   GAhO-pre
   GAhO-post

   GEhU-clause
   GEhU-pre
   GEhU-post

   GI-clause
   GI-pre
   GI-post

   GIhA-clause
   GIhA-pre
   GIhA-post

   GOI-clause
   GOI-pre
   GOI-post

   GOhA-clause
   GOhA-pre
   GOhA-post

   GUhA-clause
   GUhA-pre
   GUhA-post

   I-clause
   I-pre
   I-post

   JA-clause
   JA-pre
   JA-post

   JAI-clause
   JAI-pre
   JAI-post

   JOhI-clause
   JOhI-pre
   JOhI-post

   JOI-clause
   JOI-pre
   JOI-post

   KE-clause
   KE-pre
   KE-post

   KEhE-clause
   KEhE-pre
   KEhE-post

   KEI-clause
   KEI-pre
   KEI-post

   KI-clause
   KI-pre
   KI-post

   KOhA-clause
   KOhA-pre
   KOhA-post

   KU-clause
   KU-pre
   KU-post

   KUhE-clause
   KUhE-pre
   KUhE-post

   KUhO-clause
   KUhO-pre
   KUhO-post

   LA-clause
   LA-pre
   LA-post

   LAU-clause
   LAU-pre
   LAU-post

   LAhE-clause
   LAhE-pre
   LAhE-post

   LE-clause
   LE-pre
   LE-post

   LEhU-clause

   LI-clause
   LI-pre
   LI-post

   LIhU-clause
   LIhU-pre
   LIhU-post

   LOhO-clause
   LOhO-pre
   LOhO-post

   LOhU-clause
   LOhU-pre
   LOhU-post

   LU-clause
   LU-pre
   LU-post

   LUhU-clause
   LUhU-pre
   LUhU-post

   MAhO-clause
   MAhO-pre
   MAhO-post

   MAI-clause
   MAI-pre
   MAI-post

   ME-clause
   ME-pre
   ME-post

   MEhU-clause
   MEhU-pre
   MEhU-post

   MOhE-clause
   MOhE-pre
   MOhE-post

   MOhI-clause
   MOhI-pre
   MOhI-post

   MOI-clause
   MOI-pre
   MOI-post

   NA-clause
   NA-pre
   NA-post

   NAI-clause
   NAI-pre
   NAI-post

   NAhE-clause
   NAhE-pre
   NAhE-post

   NAhU-clause
   NAhU-pre
   NAhU-post

   NIhE-clause
   NIhE-pre
   NIhE-post

   NIhO-clause
   NIhO-pre
   NIhO-post

   NOI-clause
   NOI-pre
   NOI-post

   NU-clause
   NU-pre
   NU-post

   NUhA-clause
   NUhA-pre
   NUhA-post

   NUhI-clause
   NUhI-pre
   NUhI-post

   NUhU-clause
   NUhU-pre
   NUhU-post

   PA-clause
   PA-pre
   PA-post

   PEhE-clause
   PEhE-pre
   PEhE-post

   PEhO-clause
   PEhO-pre
   PEhO-post

   PU-clause
   PU-pre
   PU-post

   RAhO-clause
   RAhO-pre
   RAhO-post

   ROI-clause
   ROI-pre
   ROI-post

   SA-clause

   SE-clause
   SE-pre
   SE-post

   SEI-clause
   SEI-pre
   SEI-post

   SEhU-clause
   SEhU-pre
   SEhU-post

   SI-clause
   SI-pre
   SI-post

   SOI-clause
   SOI-pre
   SOI-post

   SU-clause
   SU-pre
   SU-post

   TAhE-clause
   TAhE-pre
   TAhE-post

   TEhU-clause
   TEhU-pre
   TEhU-post

   TEI-clause
   TEI-pre
   TEI-post

   TO-clause
   TO-pre
   TO-post

   TOI-clause
   TOI-pre
   TOI-post

   TUhE-clause
   TUhE-pre
   TUhE-post

   TUhU-clause
   TUhU-pre
   TUhU-post

   UI-clause
   UI-pre
   UI-post

   VA-clause
   VA-pre
   VA-post

   VAU-clause
   VAU-pre
   VAU-post

   VEI-clause
   VEI-pre
   VEI-post

   VEhO-clause
   VEhO-pre
   VEhO-post

   VUhU-clause
   VUhU-pre
   VUhU-post

   VEhA-clause
   VEhA-pre
   VEhA-post

   VIhA-clause
   VIhA-pre
   VIhA-post

   VUhO-clause
   VUhO-pre
   VUhO-post

   XI-clause
   XI-pre
   XI-post

   ;Y-clause

   ZAhO-clause
   ZAhO-pre
   ZAhO-post

   ZEhA-clause
   ZEhA-pre
   ZEhA-post

   ZEI-clause
   ZEI-pre
   ZEI-post

   ZI-clause
   ZI-pre
   ZI-post

   ZIhE-clause
   ZIhE-pre
   ZIhE-post

   ZO-clause
   ZO-pre
   ZO-post

   ZO-pre
   ZOI-clause
   ZOI-pre
   ZOI-post

   ZOhU-clause
   ZOhU-pre
   ZOhU-post

   zoi-open
   zoi-word
   zoi-close)

(import chicken)
(import scheme)

(require-library genturfahi)

(import genturfahi)

(include "c0re.scm")
(include "samselpla.scm"))
