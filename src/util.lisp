(in-package :cl-user)

(uiop:define-package cl-transmission.util
    (:use :cl :rutils.readtable)
  (:import-from :alexandria
                #:define-constant))
(in-package :cl-transmission.util)

(named-readtables:in-readtable rutils.readtable:rutils-readtable)

(defmacro define-export-macro (type)
  (let* ((name (intern (format nil "~A-EXPORT" (symbol-name type)))))
    `(progn
       (export ',name)
       (defmacro ,name (exported-name args &body body)
         `(progn
            (export ',exported-name)
            (,',type ,exported-name ,args ,@body))))))

(define-export-macro defclass)
(define-export-macro defmacro)
(define-export-macro defun)
(define-export-macro defgeneric)
(define-export-macro define-constant)
(define-export-macro define-condition)

(defmacro the-check (type val)
  (rutils:once-only (val)
    `(progn
       (check-type ,val ,type)
       (the ,type ,val))))

(defun-export make-keyword (str)
  (check-type str string)
  (intern str #.(find-package :keyword)))

(defun-export string->keyword (string)
  (check-type string string)
  (the-check symbol
             (make-keyword
              (string-upcase
               (cl-ppcre:regex-replace-all "[A-Z]" string "-\\&")))))

(defun-export plist-to-hash-table (plist
                                   &rest
                                     rest
                                   &key
                                     (convert-key #'identity)
                                     (convert-value #'identity)
                                   &allow-other-keys)
  (remf rest :convert-key)
  (remf rest :convert-value)
  (loop :with ht = (apply #'make-hash-table rest)
        :for key :in plist :by #'cddr
        :for val :in (cdr plist) :by #'cddr
        :do (rutils:sethash (funcall convert-key key)
                            ht
                            (funcall convert-value val))
        :finally (return ht)))

(declaim (inline contains-key))
(defun-export contains-key (key hash-table)
  (rutils:2nd (gethash key hash-table)))
