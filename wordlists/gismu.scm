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
{(no-memoize '("CRLF" "EOL"))}

cfari         <- `((!CRLF .)* CRLF) lerpinsle* `FAhO

lerpinsle     <- `canlu-lerfu
                 ( gismu
                 / relerfu-rafsi `cicanlu-lerfu
                 / cilerfu-rafsi `recanlu-lerfu
                 / volerfu-rafsi `canlu-lerfu )
                 `canlu-lerfu
                 ( cilerfu-rafsi / `cicanlu-lerfu )
                 `canlu-lerfu
                 ( cilerfu-rafsi / `cicanlu-lerfu )
                 `canlu-lerfu
                 ( cilerfu-rafsi `canlu-lerfu
                 / volerfu-rafsi
                 / `vocanlu-lerfu )
                 `canlu-lerfu
                 short
                 long
                 `CRLF?
              -> {(lambda lerpinsle (assq 'gismu lerpinsle))}

gismu         <- [[:jbole'u:]]{5} -> {(lambda (gismu) `(gismu ,gismu))}

relerfu-rafsi <- [[:jbole'u:]]{2} -> {(lambda (rafsi) `(rafsi ,rafsi))}

cilerfu-rafsi <- [[:jbole'u:]]{3} -> {(lambda (rafsi) `(rafsi ,rafsi))}

volerfu-rafsi <- [[:jbole'u:]]{4} -> {(lambda (rafsi) `(rafsi ,rafsi))}

short         <- .{21} -> {(lambda (lerfu) `(short lerfu))}

long          <- (!EOL .)+ -> {(lambda (lerfu) `(long ,lerfu))}

canlu-lerfu   <- [[:space:]]
recanlu-lerfu <- [[:space:]]{2}
cicanlu-lerfu <- [[:space:]]{3}
vocanlu-lerfu <- [[:space:]]{4}

EOL           <- CRLF
               / &FAhO

CRLF          <- CR LF / CR / LF
CR            <- #\return
LF            <- #\linefeed

FAhO          <- !.
EOS
)

(let* ((samselpla (genturfahi-peg gerna))
       (gerna     (genturfahi (eval samselpla)))
       (gismu     (call-with-input-file "wordlists/gismu.txt" gerna)))

  (gismu:drop-table)
  (gismu:create-table)

  (call-with-values
    gismu:gen-insert
    (lambda (insert cleanup)
      (map (compose insert cadr) (remove (curry eq? #f) gismu))
      (cleanup))))
