#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

;; Serializing and Deserializing works with the hope that any and all output
;; formats will support: Arrays, Hash Tables, Strings and Numbers.
;;
;; By default the functions defined here will only handle the following types:
;; string, symbol, hash-table, vector, list
;;
;; Adding support for other types will have to happen through the macros:
;; define-serializer, define-deserializer,
;; define-deserializer, define-string-serializer

(defun escape (string &optional (char #\:))
  (cl-ppcre:regex-replace-all (string char) string (format NIL "\\~a" char)))

(defun unescape (string &optional (char #\:))
  (cl-ppcre:regex-replace-all (format NIL "\\\\~a" char) string (string char)))

(defun split-escaped (string &optional (char #\:))
  (cl-ppcre:split (format NIL "(?<!\\\\)~a" char) string))

(defgeneric serialize (object)
  (:documentation "")
  (:method (object) object))

(defmacro define-string-serializer ((ident-char object-type object-var) &body body)
  `(defmethod serialize ((,object-var ,object-type))
     (concatenate 'string ,(string ident-char) (progn ,@body))))

(define-string-serializer (#\- string string)
  string)

(define-string-serializer (#\+ symbol symbol)
  (format NIL "~a:~a"
          (escape (package-name (symbol-package symbol)))
          (escape (symbol-name symbol))))

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
    (deserialize-string (aref string 0) (subseq string 1)))
  (:method ((vector vector))
    (deserialize-object (find-symbol (aref vector 0) "KEYWORD") (subseq vector 1)))
  (:method (object) object))

(defgeneric deserialize-string (ident-char string)
  (:documentation ""))

(defmacro define-string-deserializer ((ident-char string) &body body)
  `(defmethod deserialize-string ((,(gensym "CHAR") (eql ,ident-char)) ,string)
     ,@body))

(define-string-deserializer (#\- string)
  string)

(define-string-deserializer (#\+ string)
  (let* ((parts (split-escaped string))
         (package (find-package (first parts)))
         (symbol-name (unescape (second parts))))
    (or (find-symbol symbol-name package)
        (intern symbol-name package))))

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
