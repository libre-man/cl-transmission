;;-*- mode: lisp -*-

(in-package :cl-user)

(uiop:define-package cl-transmission
    (:use #:cl
          #:rutils.readtable
          #:rutils.anaphora
          #:cl-transmission.util
          #:cl-transmission.constants)
  (:import-from :rutils
                #:sethash
                #:length=
                #:hash-table-from-plist))
(in-package :cl-transmission)

(declaim #.+default-optimizations+)

(named-readtables:in-readtable rutils.readtable:rutils-readtable)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\d (lambda (s c n)
             (declare (ignore c n))
             (format nil (read s)))))

(setf drakma:*drakma-default-external-format* :utf8)
(pushnew '("application" . "json") drakma:*text-content-types*)

(deftype port-number ()
  '(integer 1 65535))

(defun keyword-all-p (keyword)
  (and (keywordp keyword) (eq keyword :all)))

(defun valid-torrent-get-keys-p (item)
  (when (typep item 'sequence)
    (block nil
      (map 'nil
           ^(unless (contains-key % +transmission-get-params+)
              (return nil))
           item)
      t)))

(defun valid-torrent-add-keys-p (item)
  (and (typep item 'sequence)
       (block nil
         (let ((res nil))
           (alexandria:doplist (key val item res)
             (when (or (eql key :filename) (eql key :metainfo))
               (unless (and (typep val 'string) (not res))
                 (return nil))
               (setf res t)))))))

(defun no-duplicates-p (item)
  (when (typep item 'sequence)
    (block nil
      (let ((seen #h(equal)))
        (map 'nil
             ^(if (rutils:2nd (gethash % seen))
                  (return nil)
                  (sethash % seen t))
             item)
        t))))

(defclass-export transmission-connection ()
  ((protocol
    :initarg :protocol
    :type string
    :initform "http"
    :documentation #d"The protocol to use to contact the server probably ~
\"http\" or \"https\".")
   (host
    :initarg :host
    :type string
    :initform "localhost"
    :documentation "The host of the transmission server.")
   (port
    :initarg :port
    :type port-number
    :initform 9091
    :documentation "The port of the transmission server.")
   (url
    :initarg :url
    :type string
    :initform "transmission/rpc"
    :documentation #d"The part of the url of the transmission server after the ~
port.")
   (credentials
    :initarg :credentials
    :type (or null list)
    :initform nil
    :documentation #d"he basic authentication for the transmission server. Use ~
NIL if no auth is needed.")
   (session-id
    :type string
    :initform ""
    :documentation #d"The current \"X-Transmission-Session-Id\" used by the ~
transmission server.")))

(defmacro def-torrent-request (method lambda-list (&key ignore-keys-p) &body body)
  (let* ((name (intern (concatenate 'string "TRANSMISSION-" (symbol-name method))))
         (method-string (concatenate 'string "torrent-" (string-downcase (symbol-name method))))
         (documentation (if (and (stringp (car body)) (not (length= body 1)))
                            (car body)
                            nil))
         (body (if documentation (cdr body) body))
         (declarations (if (eql (caar body) 'declare) (car body) nil))
         (body (if declarations (cdr body) body))
         (method-list (cons '(conn transmission-connection)
                            (if ignore-keys-p
                                (loop :for item :in lambda-list
                                      :for key-seen = (eql item '&key)
                                        :then (or key-seen (eql item '&key))
                                      :when (eql item '&key)
                                        :append (list '&rest 'all-keys '&key) :into res
                                      :when (or (not key-seen)
                                                (listp item))
                                        :collect item :into res
                                      :finally (return (append res
                                                               (if key-seen
                                                                   '(&allow-other-keys)
                                                                   '(&rest all-keys)))))
                                lambda-list)))
         (lambda-list (loop :for item :in lambda-list
                            :collect (if (listp item)
                                         (car item)
                                         item))))
    `(defgeneric-export ,name ,(cons 'connection lambda-list)
       (:documentation ,documentation)
       (:method ,method-list
         ,declarations
         (let ((transmission-method-name ,method-string))
           ,@body)))))

(defmacro def-torrent-action-request (action &optional docstring)
  (check-type docstring (or null string))
  `(def-torrent-request ,action (ids) (:ignore-keys-p nil)
     ,@(when docstring
         (list (concatenate 'string docstring "

The torrents are specified by \"IDS\", which should be a list of torrent
designators, a torrent designator is a torrent-id or a torrent sha1 string, or
it should be the string \"recently-active\" (case sensitive).

This function will return nothing of value.")))
     (check-type ids sequence)
     (transmission-request conn
                           transmission-method-name
                           #h(equal "ids" ids))
     (values)))

(defun get-transmission-host (conn)
  (with-slots (protocol host port url) conn
    (rutils:fmt "~A://~A:~A/~A"
                protocol
                host
                port
                url)))

(defun update-session-id (conn headers)
  (with-slots (session-id-lock session-id) conn
    (setf session-id (cdr (assoc :x-transmission-session-id
                                 headers)))))

(define-condition-export transmission-error (error)
  ((response :initarg :response :reader transmission-error-response))
  (:documentation #d"The general error used when we could not complete the ~
request to the transmission server.")
  (:report (lambda (condition stream)
             (format stream
                     "Transmission server signaled an error: \"~A\"."
                     (transmission-error-response condition)))))

(defun %transmission-request (conn method arguments)
  (check-type conn transmission-connection)
  (check-type method string)
  (check-type arguments (or hash-table list))
  (flet ((get-res (url content)
           (drakma:http-request url
                                :method :post
                                :basic-authorization (slot-value conn 'credentials)
                                :additional-headers `(("X-Transmission-Session-Id" . ,(slot-value conn 'session-id)))
                                :content content)))
    (let* ((content-hash #h(equalp
                            "method" method
                            "arguments" arguments))
           (content (jonathan:to-json content-hash))
           (url (get-transmission-host conn)))
      (multiple-value-bind (res code headers)
          (get-res url content)
        (when (= code 409)
          (update-session-id conn headers)
          (multiple-value-bind (new-res new-code)
              (get-res url content)
            (setf code new-code)
            (setf res new-res)))
        (setf res (if (= code 200)
                      (jonathan:parse res
                                      :as :hash-table
                                      :junk-allowed t
                                      :keyword-normalizer #'string->keyword
                                      :normalize-all t)
                      #h(:result res)))
        (let* ((arguments (gethash :arguments res))
               (error-string (gethash :result res)))
          (if (equal error-string "success")
              arguments
              (error 'transmission-error :response error-string)))))))

(defun transmission-request (conn method arguments)
  (loop :thereis (with-simple-restart (retry-request "Retry the request to ~
transmission server with the same arguments")
                   (%transmission-request conn method arguments))))

(def-torrent-action-request start
  "Start the given torrents by adding them to the appropriate queue.")
(def-torrent-action-request start-now
  "Start the given torrents directly bypassing the queues.")
(def-torrent-action-request stop
  "Stop (pause) the given torrents.")
(def-torrent-action-request verify
  "Queue the given torrents for verification.")
(def-torrent-action-request reannounce
  "Ask the tracker for more peers for the given torrents.")

(def-torrent-request set (ids &key bandwidth-priority download-limit
                              download-limited files-wanted files-unwanted
                              honors-session-limits location peer-limit
                              priority-high priority-low priority-normal
                              queue-position seed-idle-limit seed-idle-mode
                              seed-ratio-limit seed-ratio-mode tracker-add
                              tracker-remove tracker-replace upload-limit
                              upload-limited)
    (:ignore-keys-p t)
  "Set the specified key to the given value for the given \"IDS\".

\"IDS\" should be a list of torrent designators, where a torrent designator is a
torrent-id or a torrent sha1 string, or it should be \":ALL\" to set this value
for all the torrents on the server.

For the exact meaning of the keys see the rpc spec. Please note that leaving a
key unspecified is not the same as passing an empty array. Also note that
passing \"NIL\" will result in sending an empty array, not the boolean
\"false\". To send the boolean \"false\" pass \":FALSE\" as keyword.

This function will return nothing of value."
  (check-type ids (and (not null) (or list (satisfies keyword-all-p))))
  (let* ((args (plist-to-hash-table
                all-keys
                :test 'equal
                :convert-key ^(gethash % +transmission-set-params+))))
    (sethash (gethash :ids +transmission-set-params+)
             args
             (if (eql ids :all) #() ids))
    (transmission-request conn
                          transmission-method-name
                          args)
    (values)))

(def-torrent-request get (fields &key (ids :all) strict) (:ignore-keys-p nil)
  "Get the fields specified in \"FIELDS\" for the given \"IDS\".

\"IDS\" should be a list of torrent designators, where a torrent designator is a
torrent-id or a torrent sha1 string, or it should be \":ALL\" to set this value
for all the torrents on the server.

The specified fields should be a sequence of keywords, if \"STRICT\" is non NIL
we will check if it does not contain any illegal keywords. All legal keywords
are specified in \"CL-TRANSMISSION.CONSTANTS:+TRANSMISSION-GET-PARAMS+\"

The first return value is a list of active torrents. The second return value is
a list of removed torrents. Torrents are represented by a hash-table containing
the specified fiels normalized as keywords."
  (check-type ids (or null sequence string (satisfies keyword-all-p)))
  (check-type fields sequence)
  (when strict
    (check-type fields (and (satisfies valid-torrent-get-keys-p)
                            (satisfies no-duplicates-p))))
  (let* ((string-fields (map 'vector
                             ^(gethash % +transmission-get-params+)
                             fields))
         (string-fields (remove nil string-fields))
         (args #h(equalp "fields" string-fields
                         "ids" ids))
         (_ (when (eql ids :all) (remhash "ids" args)))
         (res (transmission-request conn
                                    transmission-method-name
                                    args)))
    (declare (ignore _))
    (values (gethash :torrents res)
            (gethash :removed res))))

(def-torrent-request add (&key cookies download-dir filename metainfo paused
                               peer-limit bandwidth-priority files-wanted
                               files-unwanted priority-high priority-low
                               priority-normal)
    (:ignore-keys-p t)
  "Add a new torrent specified the given fields.

The torrent is given in \"METAINFO\" or \"FILENAME\". \"METAINFO\" should be a
base64-encoded torrent file while \"FILENAME\" should be a filename of torrent
file or a magnet link. For the other options and more detailed descriptions see
the rpc specifications.

The first return value is a hash-table with fields for the id, \":ID\", name,
\":NAME\" and hash string, \":HASH-STRING\", of the added torrent. The second
return value is \":TORRENT-ADDED\" if a new torrent was added and
\":TORRENT-DUPLICATE\" if no new torrent was added."
  (check-type all-keys (satisfies valid-torrent-add-keys-p))
  (let* ((res (transmission-request conn
                                    transmission-method-name
                                    (plist-to-hash-table
                                     all-keys
                                     :test 'equal
                                     :convert-key ^(gethash % +transmission-add-params+)))))
    (values (car (rutils:hash-table-vals res))
            (car (rutils:hash-table-keys res)))))

(def-torrent-request remove (&key ids delete-local-data) (:ignore-keys-p nil)
  "Remove the torrents with the given \"IDS\" and maybe delete the local data.

\"IDS\" should be a list of torrent designators, where a torrent designator is a
torrent-id or a torrent sha1 string, or it should be the string
\"recently-active\" (case sensitive).

\"DELETE-LOCAL-DATA\" should parse to a json boolean. \"NIL\" will be converted
to \":FALSE\" which will parse the false boolean in json.

This function does not return anything useful.
"
  (check-type ids (or sequence string))
  (setf delete-local-data (or delete-local-data :false))
  (transmission-request conn
                        transmission-method-name
                        #h(equal "ids" ids
                                 "delete-local-data" delete-local-data))
  (values))

(def-torrent-request set-location (ids location &optional (move :false)) (:ignore-keys-p nil)
  "Move the \"IDS\" to the new \"LOCATION\" moving the old if \"MOVE\" is true.

\"IDS\" should be a list of torrent designators, where a torrent designator is a
torrent-id or a torrent sha1 string, or it should be the string
\"recently-active\" (case sensitive).

\"LOCATION\" is the new location of the specified torrents. It should parse to a
json string by jonathan.

\"MOVE\" should parse to a json boolean. \"NIL\" will be converted to \":FALSE\"
which will parse the false boolean in json.

This function does not return anything of value."
  (check-type ids (or sequence string))
  (transmission-request conn
                        transmission-method-name
                        #h(equal "ids" ids
                                 "location" location
                                 "move" move))
  (values))

(def-torrent-request rename-path (id path name) (:ignore-keys-p nil)
  "Rename the \"PATH\" of given torrent by \"ID\" to \"NAME\"

The \"ID\" should be a torrent id, sha1 hash string or the string
\"recently-active\" (case sensitive).

The torrents \"PATH\" will be renamed to \"NAME\", some examples (modified from
transmission.h):

  Consider a torrent, with id 0, which has two files with the names
  \"frobnitz-linux/checksum\" and \"frobnitz-linux/frobnitz.iso\".

  If we would call \"(TRANSMISSION-RENAME-PATH 0 \"frobnitz-linux\", \"foo\")\" then
  the \"frobnitz-linux\" folder will be renamed to \"foo\" in both torrents.
  However if we would call \"(TRANSMISSION-RENAME-PATH 0
  \"frobnitz-linux/checksum\", \"foo\")\" then only \"frobnitz-linux/checksum\"
  will be renamed to \"frobnitz-linux/foo\" while
  \"frobnitz-linux/frobnitz.iso\" will remain unchanged.

This value does not return anything value."
  (check-type id (or integer string))
  (when (string= id "recently-active")
    (setf id (list id)))
  (transmission-request conn
                        transmission-method-name
                        #h(equal "ids" id
                                 "path" path
                                 "name" name))
  (values))

#|
Local Variables:
eval: (font-lock-add-keywords nil '(("(\\(\\(def-torrent-\\(action-\\)?request\\)\\)\\s \\(\\(?:\\s_\\|\\sw\\)+\\)" (1 font-lock-keyword-face) (4 font-lock-function-name-face))))
End:
|#
