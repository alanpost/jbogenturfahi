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

(define jbogenturfahi-db (sql:open (jbogenturfahi-db-file)))

(define (cmavo:drop-table)
  (sql:drop-table jbogenturfahi-db #<<EOS
drop table if exists cmavo;
EOS
))

(define (gismu:drop-table)
  (sql:drop-table jbogenturfahi-db #<<EOS
drop table if exists gismu;
EOS
))

(define (rafsi:drop-table)
  (sql:drop-table jbogenturfahi-db #<<EOS
drop table if exists rafsi;
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

(define (gismu:create-table)
  (sql:create-table jbogenturfahi-db #<<EOS
create table if not exists
gismu(valsi varchar(5) primary key);
EOS
))

(define (rafsi:create-table)
  (sql:create-table jbogenturfahi-db #<<EOS
create table if not exists
rafsi(rafsi varchar(4) primary key,
      valsi varchar(5) not null);
EOS
))


(define (cmavo:gen-insert)
  (sql:gen-insert jbogenturfahi-db #<<EOS
insert into cmavo('valsi', 'selmaho', 'series')
values(?, ?, ?);
EOS
))

(define (gismu:gen-insert)
  (sql:gen-insert jbogenturfahi-db #<<EOS
insert into gismu('valsi')
values(?);
EOS
))

(define (rafsi:gen-insert)
  (sql:gen-insert jbogenturfahi-db #<<EOS
insert into rafsi('rafsi', 'valsi')
values(?, ?);
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

(define (gismu:gen-select-list)
  (sql:gen-select-list jbogenturfahi-db #<<EOS
select   valsi
from     gismu
order by valsi
EOS
))

(define (rafsi:gen-select-list)
  (sql:gen-select-list jbogenturfahi-db #<<EOS
select   rafsi,
         valsi
from     rafsi
order by rafsi
EOS
))
