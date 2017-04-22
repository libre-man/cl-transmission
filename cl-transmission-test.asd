#|
This file is a part of cl-transmission project.
Copyright (c) 2017 Thomas Schaper (Thomas@libremail.nl)
|#

(defsystem "cl-transmission-test"
  :author "Thomas Schaper"
  :license ""
  :depends-on ("cl-transmission"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "cl-transmission"))))
  :description "Test system for cl-transmission"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
