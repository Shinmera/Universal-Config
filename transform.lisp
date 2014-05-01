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
  (:method ((string string))
    (format NIL "-~a" string))
  (:method ((symbol symbol))
    (format NIL "+~a:~a"
            (escape (package-name (symbol-package symbol)))
            (escape (symbol-name symbol))))
  (:method (object) object))

(defmacro define-serializer ((object-type object-var &optional return-vector) &body body)
  (let ((result (gensym "RESULT")))
    `(defmethod serialize ((,object-var ,object-type))
       (let ((,result (progn ,@body)))
         ,(if return-vector
              `(concatenate 'vector '(,(string object-type)) ,result)
              `(make-array 2 :initial-contents (list ,(string object-type) ,result)))))))

(define-serializer (vector vector T)
  (map 'vector #'serialize vector))

(define-serializer (list list T)
  (map 'vector #'serialize list))

(define-serializer (hash-table table T)
  (let ((r (make-hash-table :test 'equal)))
    (loop for k being the hash-keys of table
          for v being the hash-values of table
          do (setf (gethash (serialize k) r)
                   (serialize v))
          finally (return r))
    (make-array 2 :initial-contents (list (string (hash-table-test table)) r))))

(defgeneric deserialize (object)
  (:documentation "")
  (:method ((string string))
    (if (char= (aref string 0) #\-)
        (subseq string 1)
        (destructuring-bind (package-name symbol-name) (split-escaped (subseq string 1))
          (let ((package (find-package package-name)))
            (or (find-symbol symbol-name package)
                (intern symbol-name package))))))
  (:method ((vector vector))
    (deserialize-object (find-symbol (aref vector 0) "KEYWORD") (subseq vector 1)))
  (:method (object) object))

(defgeneric deserialize-object (type object)
  (:documentation ""))

(defmacro define-deserializer ((object-type object-var &optional expect-vector) &body body)
  (let ((objtemp (gensym "OBJECT")))
    `(defmethod deserialize-object ((,(gensym "TYPE") (eql ,(intern (string object-type) "KEYWORD"))) ,objtemp)
       (let ((,object-var ,(if expect-vector objtemp `(aref ,objtemp 0))))
         ,@body))))

(define-deserializer (list array T)
  (map 'list #'deserialize array))

(define-deserializer (vector array T)
  (map 'vector #'deserialize array))

(define-deserializer (hash-table array T)
  (let* ((type (aref array 0))
         (map (aref array 1))
         (res (make-hash-table :test (cond ((string= type "EQ") 'eq)
                                           ((string= type "EQL") 'eql)
                                           ((string= type "EQUAL") 'equal)
                                           ((string= type "EQUALP") 'equalp)
                                           (T (error "Unknown HASH-TABLE test specifier: ~a" type))))))
    (loop for k being the hash-keys of map
          for v being the hash-values of map
          do (setf (gethash (deserialize k) res)
                   (deserialize v)))
    res))
