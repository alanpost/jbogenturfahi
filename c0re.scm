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


;; curry - supply some arguments to |f|.
;;
;; (curry f a ...)
;; (f a ... ...)
;;
;; basic curry function
;;
(define (curry f . a)
  (lambda x
    (apply f (append a x))))


;; map-apply - apply each element in a list to function
;;
;; (map-apply f l)
;;
(define (map-apply f l)
  (map (lambda (x) (apply f x)) l))


;; seq - return a sequence of numbers as a list
;;
;; (seq f l)
;;
(define (seq f l)
  (if (>= f l)
      '()
      (cons f (seq (+ 1 f) l))))


;; until -- call until predicate returns |#f|.
;;
;; (until p f)
;; (p)
;; (f)
;;
;; call |p|, and if the result is not |#f|,
;; call |f| and recurse.  return the results
;; from |f| as a list, or |'()| if |p| never
;; returns non-|#f|.
;; 
(define (until p f)
  (if (p) (cons (f) (until p f)) '()))
