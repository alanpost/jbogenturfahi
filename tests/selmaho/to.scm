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

(define (to)
  ;; |to ... toi| is parsed in |free|, and can appear in many places.
  ;;

  ; without terminator
  ;
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text)))
         "to")

  (mapti (text (TO-clause (cmavo (TO "to'i"))
                          (text)))
         "to'i")


  ; with terminator
  ;
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text)
                          (TOI-clause (cmavo (TOI "toi")))))
         "to toi")

  (mapti (text (TO-clause (cmavo (TO "to'i"))
                          (text)
                          (TOI-clause (cmavo (TOI "toi")))))
         "to'i toi")


  ; nesting
  ;
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to")) (text)))))
         "to to")

  (mapti (text (TO-clause (cmavo (TO "to'i"))
                          (text (TO-clause (cmavo (TO "to'i")) (text)))))
         "to'i to'i")
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to'i")) (text)))))
         "to to'i")

  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to'i")) (text)))))
         "to to'i")

  (mapti (text (TO-clause
                 (cmavo (TO "to"))
                 (text (TO-clause (cmavo (TO "to"))
                       (text (TO-clause (cmavo (TO "to")) (text)))))))
         "to to to")
  (mapti (text (TO-clause
                 (cmavo (TO "to'i"))
                 (text (TO-clause (cmavo (TO "to'i"))
                       (text (TO-clause (cmavo (TO "to'i")) (text)))))))
         "to'i to'i to'i")
  (mapti (text (TO-clause
                 (cmavo (TO "to"))
                 (text (TO-clause (cmavo (TO "to'i"))
                       (text (TO-clause (cmavo (TO "to")) (text)))))))
         "to to'i to")
  (mapti (text (TO-clause
                 (cmavo (TO "to'i"))
                 (text (TO-clause (cmavo (TO "to"))
                       (text (TO-clause (cmavo (TO "to'i")) (text)))))))
         "to'i to to'i")


  ; nesting with terminators.  terminators bind to the closest
  ; marker.   [citation needed.]
  ;
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to"))
                                           (text)
                                           (TOI-clause (cmavo (TOI "toi")))))))
         "to to toi")
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to'i"))
                                           (text)
                                           (TOI-clause (cmavo (TOI "toi")))))))
         "to to'i toi")
  (mapti (text (TO-clause (cmavo (TO "to'i"))
                          (text (TO-clause (cmavo (TO "to"))
                                           (text)
                                           (TOI-clause (cmavo (TOI "toi")))))))
         "to'i to toi")

  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to"))
                                           (text)
                                           (TOI-clause (cmavo (TOI "toi")))))
                          (TOI-clause (cmavo (TOI "toi")))))
         "to to toi toi")
  (mapti (text (TO-clause (cmavo (TO "to"))
                          (text (TO-clause (cmavo (TO "to'i"))
                                           (text)
                                           (TOI-clause (cmavo (TOI "toi")))))
                          (TOI-clause (cmavo (TOI "toi")))))
         "to to'i toi toi")
  (mapti (text (TO-clause (cmavo (TO "to'i"))
                          (text (TO-clause (cmavo (TO "to"))
                                           (text)
                                           (TOI-clause (cmavo (TOI "toi")))))
                          (TOI-clause (cmavo (TOI "toi")))))
         "to'i to toi toi")
  0)

(test-group "selma'o TO"
  (to))
