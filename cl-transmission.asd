#|
This file is a part of cl-transmission project.
Copyright (c) 2017 Thomas Schaper (Thomas@libremail.nl)
|#

#|
Author: Thomas Schaper (Thomas@libremail.nl)
|#

(asdf:defsystem "cl-transmission"
  :version "0.1"
  :author "Thomas Schaper"
  :license "MIT"
  :depends-on ("rutils"
               "drakma"
               "named-readtables"
               "cl-ppcre"
               "uiop"
               "jonathan")
  :components ((:module "src"
                :components
                ((:file "cl-transmission" :depends-on ("util" "constants"))
                 (:file "constants" :depends-on ("util"))
                 (:file "util"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-transmission-test))))
