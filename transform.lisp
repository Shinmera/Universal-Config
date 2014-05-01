#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

;; Serializing and Deserializing works with the hope that any and all output
;; formats will support: Vectors, Hash Tables, Strings and Numbers.
;;
;; By default the functions defined here will only handle the following types:
;; string, symbol, hash-table, vector, list
;;
;; Adding support for other types will have to happen through the macros:
;; define-serializer, define-deserializer,
;; define-deserializer, define-string-serializer

(defvar *fallback-serializer* #'identity)
(defvar *fallback-deserializer* #'identity)

(defun escape (string &optional (char #\:))
  "Escape all instances of CHAR in the string that match CHAR with a backslash."
  (cl-ppcre:regex-replace-all (string char) string (format NIL "\\~a" char)))

(defun unescape (string &optional (char #\:))
  "Unescape all backslash escaped instances of CHAR. "
  (cl-ppcre:regex-replace-all (format NIL "\\\\~a" char) string (string char)))

(defun split-escaped (string &optional (char #\:))
  "Split the string by CHAR minding backslash escaped instances."
  (cl-ppcre:split (format NIL "(?<!\\\\)~a" char) string))

(defgeneric serialize (object)
  (:documentation "Serialize the given object recursively into a format that is ready for outputting.")
  (:method (object)
    (funcall *fallback-serializer* object)))

(defmacro define-string-serializer ((ident-char object-type object-var) &body body)
  "Defines an OBJECT-TYPE to be serialized to a string.
To discern string serialized objects, an IDENT-CHAR is needed."
  `(defmethod serialize ((,object-var ,object-type))
     (concatenate 'string ,(string ident-char) (progn ,@body))))

(define-string-serializer (#\- string string)
  string)

(define-string-serializer (#\+ symbol symbol)
  (format NIL "~a:~a"
          (escape (package-name (symbol-package symbol)))
          (escape (symbol-name symbol))))

(defmacro define-serializer ((object-type object-var &optional return-vector) &body body)
  "Define an OBJECT-TYPE to be serialized. The expected value of this function should be one of HASH-TABLE, VECTOR, STRING or NUMBER.

If RETURN-VECTOR is non-NIL, the object returned should be of type VECTOR."
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
  (:documentation "Deserialize an OBJECT into a usable configuration object.")
  (:method ((string string))
    (deserialize-string (aref string 0) (subseq string 1)))
  (:method ((vector vector))
    (deserialize-object (find-symbol (aref vector 0) "KEYWORD") (subseq vector 1)))
  (:method (object)
    (funcall *fallback-deserializer* object)))

(defgeneric deserialize-string (ident-char string)
  (:documentation "Deserialize a STRING according to the IDENT-CHAR."))

(defmacro define-string-deserializer ((ident-char string) &body body)
  "Define a new IDENT-CHAR handler to deserialize a STRING with."
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

(define-string-deserializer (#\i string)
  (parse-integer string))

(define-string-deserializer (#\f string)
  (parse-float string))

(define-string-deserializer (#\c string)
  (let ((parts (split-escaped string)))
    (complex (parse-float (first parts))
             (parse-float (second parts)))))

(defgeneric deserialize-object (type object)
  (:documentation "Deserialize an OBJECT of TYPE into its usable representation.

OBJECT itself will be one of HASH-TABLE, VECTOR, STRING or NUMBER."))

(defmacro define-deserializer ((object-type object-var &optional expect-vector) &body body)
  "Define a new OBJECT-TYPE to deserialize into a usable representation.

If EXPECT-VECTOR is non-NIL, the bound OBJECT-VAR will be of type VECTOR."
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
    (etypecase map
      (hash-table (loop for k being the hash-keys of map
                        for v being the hash-values of map
                        do (setf (gethash (deserialize k) res)
                                 (deserialize v))))
      (list (loop for (k v) on map by #'cddr
                  do (setf (gethash (deserialize k) res)
                           (deserialize v))))
      (vector (loop for i below (length map)
                    for k = NIL then v
                    for v = (aref map i)
                    when (= (mod i 2) 1)
                      do (setf (gethash (deserialize k) res)
                               (deserialize v)))))
    res))
