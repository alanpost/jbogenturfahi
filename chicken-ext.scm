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

(module jbogenturfahi
  (jbogenturfahi-version
   jbogenturfahi-version-major
   jbogenturfahi-version-minor
   jbogenturfahi-version-patch

   jbogenturfahi-rafske*
   jbogenturfahi*

   jbogenturfahi-rafske
   jbogenturfahi

   cmavo:drop-table
   cmavo:create-table
   cmavo:create-index
   cmavo:gen-insert
   cmavo:gen-select-list
   cmavo:gen-select-list-w/compound
   
   gismu:drop-table
   gismu:create-table
   gismu:gen-insert
   gismu:gen-select-list
   
   rafsi:drop-table
   rafsi:create-table
   rafsi:gen-insert
   rafsi:gen-select-list
   
   jbogenturfahi-db
   jbogenturfahi-db-file)

(import chicken)
(import scheme)

(require-extension utf8)
(require-extension utf8-srfi-14)
(require-extension extras)

(require-library genturfahi)
(require-library jbogerna)

(require-library sqlite3)


(import utf8)
(import utf8-srfi-14)
(import extras)

(import genturfahi)
(import jbogerna)

(import sqlite3)

(include "jbogenturfahi.scm")
(include "version.scm")
(include "path.scm")
(include "c0re.scm")
(include "sql.scm")
(include "db.scm"))
