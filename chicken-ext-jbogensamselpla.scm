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
   comma
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
   ZOhU)

(import chicken)
(import scheme)

(require-extension srfi-13)

(import srfi-13)

(include "samselpla.scm"))
