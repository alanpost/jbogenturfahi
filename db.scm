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

(define jbogenturfahi-db (sql:open (jbogenturfahi-db-path)))

(define (cmavo:drop-table)
  (sql:drop-table jbogenturfahi-db #<<EOS
drop table if exists cmavo;
EOS
))

(define (cmavo:create-table)
  (sql:create-table jbogenturfahi-db #<<EOS
create table if not exists
cmavo(valsi varchar(9) primary key,
      selmaho varchar(6) not null,
      series int);
EOS
))

(define (cmavo:gen-insert)
  (sql:gen-insert jbogenturfahi-db #<<EOS
insert into cmavo('valsi', 'selmaho', 'series')
values(?, ?, ?);
EOS
))

(define (cmavo:gen-select-list)
  (sql:gen-select-list jbogenturfahi-db #<<EOS
select   selmaho,
         valsi
from     cmavo
order by valsi
EOS
))