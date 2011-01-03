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

(set! gerna #<<EOS
{(no-memoize "CRLF")}

cfari       <- `((!CRLF .)* CRLF) lerpinsle* `FAhO

lerpinsle   <- rafsi selmaho short long `CRLF?
            -> {(lambda (rafsi selmaho short long) `(,rafsi ,@selmaho))}

rafsi       <- [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jbole'u:][:jboca'u:]]
               [[:jboca'u:]]
            -> {(lambda lerfu
                  (string-trim-both
                    (apply string (remove (curry char=? #\space) lerfu))
                    #\.))}

selmaho     <- [[:upper:]h]+
               `#\*?
               [[:digit:]]?
               canlu
            -> {(lambda (selmaho series)
                  `(,selmaho ,(if (char? series)
                                  (string->number (string series))
                                  0)))}

short       <- (canlu-lerfu !canlu-lerfu / (!canlu-lerfu .))+
               canlu

long        <- (!EOL .)+

canlu       <- `canlu-lerfu*
canlu-lerfu <- [[:space:]]

EOL           <- CRLF
               / &FAhO

CRLF        <- CR LF / CR / LF
CR          <- #\return
LF          <- #\linefeed

FAhO        <- !.
EOS
)

(let* ((samselpla      (genturfahi-peg gerna))
       (gerna          (genturfahi (eval samselpla)))
       (cmavo          (call-with-input-file "wordlists/cmavo.txt" gerna)))

  (cmavo:drop-table)
  (cmavo:create-table)

  (call-with-values
    cmavo:gen-insert
    (lambda (insert cleanup)
      (map-apply insert cmavo)
      (cleanup))))
