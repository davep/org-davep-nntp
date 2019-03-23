;;; org-davep-nntp --- Simple NNTP client for Common Lisp.
;;
;; packages.lisp --- Defines packages for org-davep-nntp.
;; Copyright 2001-2004 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2001-2004.
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

(defpackage #:org.davep.nntp
  (:nicknames #:nntp)
  (:use #:common-lisp #:split-sequence)
  (:documentation "NNTP client classes, functions and macros")
  (:export "*DEFAULT-NNTP-HOST*"        ; Global config variables.
           "*DEFAULT-NNTP-PORT*"
           "DELIMITED-ARTICLE-ID-P"     ; Utility functions and macros.
           "TIDY-ARTICLE-ID"
           "WITH-NNTP-CLIENT"
           "NNTP-CLIENT-RESPONSE"       ; NNTP-CLIENT-RESPONSE.
           "CODE"
           "RESPONSE"
           "DATA"
           "TEMPORARY-ERROR-P"
           "PERMANENT-ERROR-P"
           "ERRORP"
           "NNTP-ARTICLE-ID"            ; NNTP-ARTICLE-ID.
           "ARTICLE-NUMBER"
           "ARTICLE-ID"
           "NNTP-GROUP"                 ; NNTP-GROUP.
           "GROUP-NAME"
           "FIRST-ARTICLE"
           "LAST-ARTICLE"
           "NNTP-GROUP-INFO"            ; NNTP-GROUP-INFO.
           "FLAGS"
           "NNTP-GROUP-DETAILS"         ; NNTP-GROUP-DETAILS.
           "ARTICLE-COUNT"
           "NNTP-XHDR-DETAILS"          ; NNTP-XHDR-DETAILS.
           "ARTICLE-ID"
           "HEADER-VALUE"
           "NNTP-CLIENT"                ; NNTP-CLIENT.
           "HOST"
           "PORT"
           "SOCKET"
           "MAKE-NNTP-CLIENT"
           "CONNECTEDP"
           "CONNECT"
           "SERVER-HELP"
           "MODE"
           "GROUP-LIST"
           "NEW-GROUPS"
           "NEW-NEWS"
           "GROUP"
           "STAT"
           "NEXT"
           "PREVIOUS"
           "HEAD"
           "BODY"
           "ARTICLE"
           "XHDRS"
           "XHDR"
           "DISCONNECT"))

;;; packages.lisp ends here.
