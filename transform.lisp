#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *default-transformer* :lisp
  "")

(defun escape (string &optional (char #\:))
  (cl-ppcre:regex-replace-all (string char) string (format NIL "\\~a" char)))

(defun split-escaped (string &optional (char #\:))
  (cl-ppcre:split (format NIL "(?<!\\\\)~a" char) string))

(defgeneric serialize (object)
  (:documentation "")
  (:method ((o string))
    (format NIL "-~a" o))
  (:method ((o symbol))
    (format NIL "+~a:~a"
            (escape (package-name (symbol-package o)))
            (escape (symbol-name o))))
  (:method ((o hash-table))
    (let ((r (make-hash-table :test 'equal)))
      (loop for k being the hash-keys of o
            for v being the hash-values of o
            do (setf (gethash (serialize k) r)
                     (serialize v))
            finally (return r))
      (make-array 3 :initial-contents (list "HASH-TABLE" (string (hash-table-test o)) r))))
  (:method ((o vector))
    (loop with length = (1+ (array-dimension o 0))
          with r = (make-array length)
          for i from 1 below length
          do (setf (aref r i) (serialize (aref o (1- i))))
          finally (return (progn (setf (aref r 0) "VECTOR") r))))
  (:method ((o list))
    (loop with length = (1+ (length o))
          with r = (make-array length)
          for i from 1 below length
          for element in o
          do (setf (aref r i) (serialize element))
          finally (return (progn (setf (aref r 0) "LIST") r))))
  (:method (o) o))

(defgeneric deserialize-primitive (object)
  )

(defgeneric deserialize (type object)
  )
