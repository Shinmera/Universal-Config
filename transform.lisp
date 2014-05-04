#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *fallback-serializer* #'identity
  "Fallback function used when no serializer method matched.

Useful for applying serializers specific to the output format.")
(defvar *fallback-deserializer* #'identity
  "Fallback function used when no deserializer method matched.

Useful for applying deserializers specific to the input format.")
(defvar *serialize-symbols* T
  "Whether symbols should be serialized into string representation.

Note that symbol plists are not serialized into strings.")
(defvar *serialize-hash-tables* T
  "Whether to serialize hash-tables into a vector and a hash-table.

This is necessary for most output formats as there is no differentiation
made between hash-table tests. Serializing them will ensure that the proper
test can be restored upon deserializing.")
(defvar *serialize-lists* T
  "Whether to serialize lists into vector representation.

This is necessary for most output formats as there are no two representations
of list- or vector-like structures.")
(defvar *serialize-numbers* T
  "Whether to serialize numbers into string representation.

This is necessary for many output formats as they do not support the variety of
number types lisp provides (ratios, floats, complex numbers).")

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

(defmethod serialize ((list list))
  (if *serialize-lists*
      (concatenate 'vector '("LIST") (map 'vector #'serialize list))
      (map 'list #'serialize list)))

(defmethod serialize ((table hash-table))
  (if *serialize-hash-tables*
      (let ((r (make-hash-table :test 'equal)))
        (loop for k being the hash-keys of table
              for v being the hash-values of table
              do (setf (gethash (serialize k) r)
                       (serialize v))
              finally (return r))
        (make-array 3 :initial-contents (list "HASH-TABLE" (string (hash-table-test table)) r)))
      (loop with r = (make-hash-table :test (hash-table-test table))
            for k being the hash-keys of table
            for v being the hash-values of table
            do (setf (gethash (serialize k) r)
                     (serialize v))
            finally (return r))))

(defmethod serialize ((symbol symbol))
  (if *serialize-symbols*
      (format NIL "+~a:~a"
              (escape (package-name (symbol-package symbol)))
              (escape (symbol-name symbol)))
      symbol))

(defmethod serialize ((number number))
  (if *serialize-numbers*
      (etypecase number
        (integer (format NIL "i~S" number))
        (float (format NIL "f~S" number))
        (ratio (format NIL "r~S" number))
        (complex (format NIL "c~S:~S" (realpart number) (imagpart number))))
      number))

(defgeneric deserialize (object)
  (:documentation "Deserialize an OBJECT into a usable configuration object.")
  (:method ((string string))
    (deserialize-string (aref string 0) (subseq string 1)))
  (:method ((vector vector))
    (deserialize-object (find-symbol (aref vector 0) "KEYWORD") (subseq vector 1)))
  (:method ((list list))
    (mapcar #'deserialize list))
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

(define-string-deserializer (#\r string)
  (let ((parts (split-escaped string #\/)))
    (/ (parse-integer (first parts))
       (parse-integer (second parts)))))

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
  (loop with res = (make-array (length array) :adjustable T :fill-pointer t)
        for i below (length array)
        do (setf (aref res i) (deserialize (aref array i)))
        finally (return res)))

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

(defmacro define-class-de/serializer (class &rest slotdefs)
  "Shorthand macro to define de/serializer methods for a class and the specified slots.

CLASS    --- A class type.
SLOTDEFS ::= SLOTDEF*
SLOTDEF  ::= SLOT-SYMBOL | (SLOT-SYMBOL INITARG-SYMBOL)"
  (let ((instance (gensym "INSTANCE"))
        (contents (gensym "CONTENTS")))
    `(progn (define-serializer (,class ,instance T)
              (let ((,contents (list ,@(loop for slotdef in slotdefs
                                             for slot = (if (listp slotdef) (car slotdef) slotdef)
                                             collect `(serialize (slot-value ,instance ',slot))))))
                (make-array ,(length slotdefs) :initial-contents ,contents)))
            (define-deserializer (,class ,instance T)
              (make-instance ',class
                             ,@(loop with list = ()
                                     for slotdef in slotdefs
                                     for arg = (if (listp slotdef) (second slotdef) (find-symbol (string slotdef) "KEYWORD"))
                                     for i from 0
                                     do (push arg list)
                                        (push `(deserialize (aref ,instance ,i)) list)
                                     finally (return (nreverse list))))))))
