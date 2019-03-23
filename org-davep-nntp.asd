;;; org-davep-nntp --- Simple NNTP client for Common Lisp.
;;
;; org-davep-nntp.asd --- asdf package defintion file.
;; Copyright 2001-2004 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2001-2004.
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

(defpackage #:org-davep-nntp-system
  (:use #:common-lisp #:asdf))

(in-package :org-davep-nntp-system)

(defsystem org-davep-nntp
  :name        "org-davep-nntp"
  :author      "Dave Pearson <davep@davep.org>"
  :maintainer  "Dave Pearson <davep@davep.org>"
  :licence     "LLGPL"
  :version     "2.0"
  :description "Simple NNTP client for Common Lisp."
  :depends-on  (:split-sequence :acl-compat)
  :components  ((:file "packages")
                (:file "nntp" :depends-on ("packages"))))

;;; org-davep-nntp.asd ends here.
