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

(define (zoi)
  ; zoi is not allowed without delimiting words.
  ;
  (narmapti "zoi")

  ; the delimiting word is not permitted to be elidded.
  ;
  (narmapti "zoi .gy. foo")
  (narmapti "zoi .gy. foo bar")
  (narmapti "zoi .gy. foo bar baz")


  ; zoi must be followed by a (potentially empty) quotation.
  ;
  (mapti ('text (_ *** '(ZOI-clause (cmavo (ZOI "zoi"))
                                    (cmavo (BY "gy"))
                                    (cmavo (BY "gy")))))
         "zoi .gy. .gy.")
  (mapti ('text (_ *** '(ZOI-clause (cmavo (ZOI "zoi"))
                                    (cmavo (BY "gy"))
                                    (non-lojban-word "foo")
                                    (cmavo (BY "gy")))))
         "zoi .gy. foo .gy.")
  (mapti ('text (_ *** '(ZOI-clause (cmavo (ZOI "zoi"))
                                    (cmavo (BY "gy"))
                                    (non-lojban-word "foo")
                                    (cmene "bar")
                                    (cmavo (BY "gy")))))
         "zoi .gy. foo bar .gy.")
  (mapti ('text (_ *** '(ZOI-clause (cmavo (ZOI "zoi"))
                                    (cmavo (BY "gy"))
                                    (non-lojban-word "foo")
                                    (cmene "bar")
                                    (cmene "baz")
                                    (cmavo (BY "gy")))))
         "zoi .gy. foo bar baz .gy.")


  ; commonly used delimiter words.
  ;
  (mapti ('text (_ *** '(ZOI-clause (cmavo (ZOI "zoi"))
                                    (cmene "kuot")
                                    (cmavo (ZOI "zoi"))
                                    (cmene "kuot"))))
         "zoi .kuot. zoi .kuot.")
  (mapti ('text (_ *** '(ZOI-clause (cmavo (ZOI "zoi"))
                                    (cmavo (ZOI "zoi"))
                                    (cmene "kuot")
                                    (cmavo (ZOI "zoi")))))
         "zoi zoi .kuot. zoi")
  0)

(test-group "selma'o ZOI"
  (zoi))
