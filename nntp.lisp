;;; nntp.lisp --- Simple NNTP client for Common Lisp
;; Copyright 2001-2004 by Dave Pearson <davep@davep.org>

;; nntp.lisp is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;; NOTE: The following code has been written while doing three things:
;;
;; o Reading RFC977.
;; o Reading RFC2980.
;; o Playing against my local NNTP server (INN).
;;
;; So, keeping this in mind, don't be surprised to find that I've either
;; misread either of the RFCs or that I've gone and done something a little
;; too INN specific. If you see anything please feel free to let me know
;; and/or supply a fix.
;;
;; You can always find the latest version of org-davep-nntp at:
;;
;;    <URL:https://github.com/davep/org-davep-nntp>

;;; Code:

(in-package :org.davep.nntp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global default variables.

(defvar *default-nntp-host* "localhost"
  "Host name to use when a default host is required.")

(defvar *default-nntp-port* 119
  "Port to use when a default port is required.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions.

(defun delimited-article-id-p (id)
  "Is an article ID delimited with <> characters?"
  (let ((length (length id)))
    (and (> length 1)
         (eq (aref id 0) #\<)
         (eq (aref id (1- length)) #\>))))

(defun tidy-article-id (id)
  "Ensure that an article ID is properly delimited."
  (if (delimited-article-id-p id)
      id
    (format nil "<~A>" id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding responses from the server.

(defclass nntp-client-response ()
  ((code     :accessor code     :initarg :code)
   (response :accessor response :initarg :response)
   (data     :accessor data     :initform nil)))

(defun make-nntp-client-response (text)
  "Take text response and create a populated instace of a `nntp-client-response'."
  (make-instance 'nntp-client-response
                 :code (parse-integer (subseq text 0 3))
                 :response (if (> (length text) 3)
                               (subseq text 4)
                             "")))

(defgeneric temporary-error-p (response)
  (:documentation "Is RESPONSE a temporary error?"))

(defmethod temporary-error-p ((response nntp-client-response))
  (< 399 (code response) 500))

(defgeneric permanent-error-p (response)
  (:documentation "Is RESPONSE a permanent error?"))

(defmethod permanent-error-p ((response nntp-client-response))
  (< 499 (code response) 600))

(defgeneric errorp (response)
  (:documentation "Is RESPONSE any kind of error?"))

(defmethod errorp ((response nntp-client-response))
  (or (temporary-error-p response)
      (permanent-error-p response)))

(defmethod print-object ((response nntp-client-response) (stream stream))
  "Format the NNTP-CLIENT-RESPONSE for easy reading when output to STREAM."
  (print-unreadable-object (response stream :type t)
    (format stream "~S" (response response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding an article ID as returned from stat-like commands.

(defclass nntp-article-id ()
  ((number :accessor article-number :initform nil)
   (id     :accessor article-id     :initform nil)))

(defun parse-stat-result (result)
  "Create an article ID object from a \"stat\" result string."
  (let ((ret (make-instance 'nntp-article-id)))
    (setf (article-number ret) (read-from-string result))
    (setf (article-id     ret) (subseq result
                                       (position "<" result :test #'string=)
                                       (1+ (position ">" result :test #'string=))))
    ret))

(defmethod print-object ((id nntp-article-id) (stream stream))
  "Format the NNTP-ARTICLE-ID for easy reading when output to STREAM."
  (print-unreadable-object (id stream :type t)
    (format stream "~S ~S" (article-number id) (article-id id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base class for group information.

(defclass nntp-group ()
  ((group-name    :accessor group-name    :initarg :group-name    :initform nil)
   (first-article :accessor first-article :initarg :first-article :initform nil)
   (last-article  :accessor last-article  :initarg :last-article  :initform nil)))

(defgeneric print-group-detail (group stream)
  (:documentation "Format the details of the GROUP to STREAM."))

(defmethod print-group-detail ((group nntp-group) (stream stream))
  (format stream "~S ~S ~S" (group-name group) (first-article group) (last-article group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding the information of a group.

(defclass nntp-group-info (nntp-group)
  ((flags :accessor group-flags :initarg :group-flags :initform "")))

(defun parse-group-info (result)
  "Create a group information object from a \"new-groups\" result string."
  (let ((result (split-sequence #\Space result)))
    (make-instance 'nntp-group-info
                   :group-name    (nth 0 result)
                   :first-article (parse-integer (nth 2 result))
                   :last-article  (parse-integer (nth 1 result))
                   :group-flags   (nth 3 result))))

(defmethod print-object ((group nntp-group-info) (stream stream))
  "Format the NNTP-GROUP-INFO for easy reading when output to STREAM."
  (print-unreadable-object (group stream :type t)
    (print-group-detail group stream)
    (format stream " ~S" (group-flags group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding the details of a group.

(defclass nntp-group-details (nntp-group)
  ((article-count :accessor article-count :initarg :article-count :initform nil)))

(defun parse-group-details (result)
  "Create a group detail object from a GROUP result string."
  (let ((result (split-sequence #\Space result)))
    (make-instance 'nntp-group-details
                   :article-count (parse-integer (nth 0 result))
                   :first-article (parse-integer (nth 1 result))
                   :last-article  (parse-integer (nth 2 result))
                   :group-name    (nth 3 result))))

(defmethod print-object ((group nntp-group-details) (stream stream))
  "Format the NNTP-GROUP-DETAILS for easy reading when output to STREAM."
  (print-unreadable-object (group stream :type t)
    (print-group-detail group stream)
    (format stream " ~S" (article-count group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding an xhdr result.

(defclass nntp-xhdr-details ()
  ((article-id   :accessor article-id   :initform nil)
   (header-value :accessor header-value :initform nil)))

(defun parse-xhdr-details (result)
  "Create a XHDR details object."
  (let ((ret (make-instance 'nntp-xhdr-details)))
    (if (and (> (length result) 1) (eq (aref result 0) #\<))
        (progn
          (setf (article-id ret)   (subseq result 0 (1+ (position ">" result :test #'string=))))
          (setf (header-value ret) (subseq result (1+ (length (article-id ret))))))
      (multiple-value-bind (number value-start) (read-from-string result)
        (setf (article-id   ret) number)
        (setf (header-value ret) (subseq result value-start))))
    ret))

(defmethod print-object ((xhdr nntp-xhdr-details) (stream stream))
  "Format the XHDR for easy reading when output to STREAM."
  (print-unreadable-object (xhdr stream :type t)
    (format stream "~S ~S" (article-id xhdr) (header-value xhdr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NNTP client class.

(defclass nntp-client ()
  ((host    :accessor host :initarg :host :initform *default-nntp-host*)
   (port    :accessor port :initarg :port :initform *default-nntp-port*)
   (socket  :accessor socket              :initform nil)))

(defmethod initialize-instance :after ((nntp-client nntp-client) &rest rest)
  (declare (ignore rest))
  ;; Allow the host to be "host:port", split the port out, set the host
  ;; again and set the new port setting.
  (let ((host (split-sequence #\: (host nntp-client))))
    (when (> (length host) 1)
      (setf (host nntp-client) (car host))
      (setf (port nntp-client) (parse-integer (cadr host))))))

(defun make-nntp-client (&key (host *default-nntp-host*) (port *default-nntp-port*))
  "Create an NNTP client."
  (make-instance 'nntp-client :host host :port port))

(defgeneric connectedp (client)
  (:documentation "Is CLIENT connected to an NNTP server?"))

(defmethod connectedp ((nntp-client nntp-client))
  (not (null (socket nntp-client))))

(defgeneric connected-check (client)
  (:documentation "Check that NNTP-CLIENT is connected to a server, error if not."))

(defmethod connected-check ((nntp-client nntp-client))
  (unless (connectedp nntp-client)
    (error "Not connected to an NNTP server")))

(defmethod print-object ((nntp-client nntp-client) (stream stream))
  "Format the NNTP-CLIENT for easy reading when output to STREAM."
  (print-unreadable-object (nntp-client stream :type t)
    (format stream "~A:~D (~:[not ~;~]connected)"
            (host nntp-client)
            (port nntp-client)
            (connectedp nntp-client))))

(defgeneric connect (client)
  (:documentation "Connect the NNTP client to the NNTP server."))

(defmethod connect :before ((nntp-client nntp-client))
  "Ensure that we're not already connected"
  (when (connectedp nntp-client)
    (error "Already connected to host ~A on port ~A" (host nntp-client) (port nntp-client))))

(defmethod connect ((nntp-client nntp-client))
  "Connect the NNTP client to the NNTP server."
  (setf (socket nntp-client) (acl-socket:make-socket :remote-host (host nntp-client) :remote-port (port nntp-client) :format :text))
  (get-short-response nntp-client))

(defmethod get-line :before ((nntp-client nntp-client))
  "Ensure that we're connected before getting a line."
  (connected-check nntp-client))

(defmethod get-line ((nntp-client nntp-client))
  "Read a line from the NNTP server."
  (let* ((line (read-line (socket nntp-client)))
         (length (length line)))
    (if (and (> length 0) (eq (aref line (1- length)) #\Return))
        (subseq line 0 (1- length))
      line)))

(defmethod put-line :before ((nntp-client nntp-client) (line string))
  "Ensure that we're connected before putting a line."
  (connected-check nntp-client))

(defmethod put-line ((nntp-client nntp-client) (line string))
  "Send a line to the NNTP server."
  (format (socket nntp-client) "~A~C~C" line #\Return #\NewLine)
  (finish-output (socket nntp-client)))

(defgeneric get-short-response (client)
  (:documentation "Get a short (one line) response from the NNTP server."))

(defmethod get-short-response ((nntp-client nntp-client))
  (make-nntp-client-response (get-line nntp-client)))

(defgeneric get-long-response (client)
  (:documentation "Get a long (multi line) response from the NNTP server."))

(defmethod get-long-response ((nntp-client nntp-client))
  (let ((response (get-short-response nntp-client)))
    (unless (errorp response)
      (setf (data response)
            (loop for line = (get-line nntp-client)
                  until (string= line ".")
                  collect line)))
    response))

(defgeneric short-command (client command)
  (:documentation "Send COMMAND to server and get its single line response."))

(defmethod short-command ((nntp-client nntp-client) (command string))
  (put-line nntp-client command)
  (get-short-response nntp-client))

(defgeneric long-command (client command)
  (:documentation "Send COMMAND to server and get its multi-line response."))

(defmethod long-command ((nntp-client nntp-client) (command string))
  (put-line nntp-client command)
  (get-long-response nntp-client))

(defgeneric server-help (client)
  (:documentation "Request help from the NNTP server."))

(defmethod server-help ((nntp-client nntp-client))
  (long-command nntp-client "help"))

(defgeneric mode (client mode)
  (:documentation "Perform a mode command on CLIENT setting mode to MODE"))

(defmethod mode ((nntp-client nntp-client) (mode string))
  (short-command nntp-client (format nil "mode ~A" mode)))

(defmethod mode ((nntp-client nntp-client) (mode (eql :reader)))
  (mode nntp-client "reader"))

(defgeneric group-list (client)
  (:documentation "Get the newsgroup list from the NNTP server."))

(defmethod group-list ((nntp-client nntp-client))
  (let ((response (long-command nntp-client "list")))
    (unless (errorp response)
      (setf (data response) (mapcar #'parse-group-info (data response))))
    response))

(defgeneric new-groups (client date-time)
  (:documentation "Get the list of new groups added to the server since DATE-TIME."))

(defmethod new-groups ((nntp-client nntp-client) (date-time string))
  "Get the list of new groups added to the server since DATE-TIME.

DATE-TIME should be a string in the format \"YYMMDD HHMMSS [GMT]\"."
  (let ((response (long-command nntp-client (format nil "newgroups ~A" date-time))))
    (unless (errorp response)
      (setf (data response) (mapcar #'parse-group-info (data response))))
    response))

(defmethod new-groups ((nntp-client nntp-client) (date-time integer))
  "Get the list of new groups added to the server since DATE-TIME.

DATE-TIME should be a time value as returned from GET-UNIVERSAL-TIME."
  (new-groups nntp-client
              (multiple-value-bind (sec min hour date mon year)
                  (decode-universal-time date-time 0)
                (format nil "~2,'0d~2,'0d~2,'0d ~2,'0d~2,'0d~2,'0d GMT" (mod year 100) mon date hour min sec))))

(defgeneric new-news (client group date time)
  (:documentation "Get the list of new articles in GROUP posted since DATE and TIME."))

(defmethod new-news ((nntp-client nntp-client) (group string) (date string) (time string))
  (long-command nntp-client (format nil "newnews ~A ~A ~A" group date time)))

(defgeneric group (client group)
  (:documentation "Select GROUP on the server connected to by CLIENT."))

(defmethod group ((nntp-client nntp-client) (group string))
  (let ((response (short-command nntp-client (format nil "group ~A" group))))
    (unless (errorp response)
      (setf (data response) (parse-group-details (response response))))
    response))

(defun stat-populate (response)
  "Populate a RESPONSE from a stat command."
  (unless (errorp response)
    (setf (data response) (parse-stat-result (response response))))
  response)

(defmethod stat ((nntp-client nntp-client) (id string))
  "Stat article by ID."
  (stat-populate (short-command nntp-client (format nil "stat ~A" (tidy-article-id id)))))

(defmethod stat ((nntp-client nntp-client) (number integer))
  "Stat article by NUMBER."
  (stat-populate (short-command nntp-client (format nil "stat ~A" number))))

(defgeneric next (client)
  (:documentation "Move to the next article."))

(defmethod next ((nntp-client nntp-client))
  (stat-populate (short-command nntp-client "next")))

(defgeneric previous (client)
  (:documentation "Move to the previous article."))

(defmethod previous ((nntp-client nntp-client))
  (stat-populate (short-command nntp-client "last")))

(defmethod head ((nntp-client nntp-client) (id string))
  "Return the header of article by ID."
  (long-command nntp-client (format nil "head ~A" (tidy-article-id id))))

(defmethod head ((nntp-client nntp-client) (number integer))
  "Return the header of article by NUMBER."
  (long-command nntp-client (format nil "head ~A" number)))

(defmethod head ((nntp-client nntp-client) (article nntp-article-id))
  "Return the header of an article."
  (head nntp-client (article-id article)))

(defmethod body ((nntp-client nntp-client) (id string))
  "Return the body of article by ID."
  (long-command nntp-client (format nil "body ~A" (tidy-article-id id))))

(defmethod body ((nntp-client nntp-client) (number integer))
  "Return the body of article by NUMBER."
  (long-command nntp-client (format nil "body ~A" number)))

(defmethod body ((nntp-client nntp-client) (article nntp-article-id))
  "Return the body of an article."
  (body nntp-client (article-id article)))

(defmethod article ((nntp-client nntp-client) (id string))
  "Return the header and body of an article as a list of lines."
  (append (data (head nntp-client id)) (list "") (data (body nntp-client id))))

(defmethod article ((nntp-client nntp-client) (number integer))
  "Return the header and body of an article as a list of lines."
  (append (data (head nntp-client number)) (list "") (data (body nntp-client number))))

(defmethod article ((nntp-client nntp-client) (article nntp-article-id))
  "Return the header and body of an article as a list of lines."
  (article nntp-client (article-id article)))

(defgeneric xhdrs (client header from to)
  (:documentation "Get header HEADER for a range of articles."))

(defmethod xhdrs ((nntp-client nntp-client) (header string) (from integer) (to integer))
  (let ((response (long-command nntp-client (format nil "xhdr ~A ~D-~D" header from to))))
    (unless (errorp response)
      (setf (data response) (mapcar #'parse-xhdr-details (data response))))
    response))

(defmethod xhdr ((nntp-client nntp-client) (header string) (number integer))
  "Get header HEADER from article NUMBER."
  (let ((result (xhdrs nntp-client header number number)))
    (unless (errorp result)
      (setf (data result) (car (data result))))
    result))

(defmethod xhdr ((nntp-client nntp-client) (header string) (id string))
  "Get hreader HEADER from article ID."
  (let ((response (long-command nntp-client (format nil "xhdr ~A ~A" header (tidy-article-id id)))))
    (unless (errorp response)
      (setf (data response) (parse-xhdr-details (car (data response)))))
    response))

(defmethod xhdr ((nntp-client nntp-client) (header string) (article nntp-article-id))
  "Get header HEADER of ARTICLE."
  (xhdr nntp-client header (article-id article)))

(defgeneric disconnect (client)
  (:documentation "Quit the NNTP session."))

(defmethod disconnect ((nntp-client nntp-client))
  (let ((result (short-command nntp-client "quit")))
    (unless (errorp result)
      (close (socket nntp-client))
      (setf (socket nntp-client) nil))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and macros.

(defmacro with-nntp-client ((client &key (host *default-nntp-host*) (port *default-nntp-port*)) &rest body)
  "Create an NNTP client called CLIENT and evaluate BODY."
  `(let ((,client (make-nntp-client :host ,host :port ,port)))
    (connect ,client)
    (unwind-protect (progn ,@body)
      (when (connectedp ,client)
        (disconnect ,client)))))

;;; nntp.lisp ends here.
