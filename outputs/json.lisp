#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config.json)

(defvar *level* 0)
(defvar *tab-size* 2)

(defun indent (stream)
  (write-string (make-string *level* :initial-element #\Space) stream))

(defun hash-table-printer (stream table)
  (format stream "{~%")
  (let ((*level* (+ *level* *tab-size*)))
    (loop for i downfrom (hash-table-count table)
          for k being the hash-keys of table
          for v being the hash-values of table
          do (indent stream)
             (format stream "~s: ~s" k v)
             (unless (= i 1)
               (write-char #\, stream))
             (format stream "~%")))
  (indent stream)
  (format stream "}"))

(defun vector-printer (stream vector)
  (format stream "[~%")
  (let ((*level* (+ *level* *tab-size*)))
    (loop for i downfrom (length vector)
          for v across vector
          do (indent stream)
             (format stream "~s" v)
             (unless (= i 1)
               (write-char #\, stream))
             (format stream "~%")))
  (indent stream)
  (format stream "]"))

(defun string-printer (stream string)
  (write-char #\" stream)
  (write-string (escape string #\") stream)
  (write-char #\" stream))

(defun symbol-printer (stream symbol)
  (if (eq symbol NIL)
      (write-string "null" stream)
      (progn
        (write-char #\" stream)
        (write-char #\+ stream)
        (write-string (escape (escape (package-name (symbol-package symbol)) #\:) #\") stream)
        (write-char #\: stream)
        (write-string (escape (escape (symbol-name symbol) #\:) #\") stream)
        (write-char #\" stream))))

(defvar +stop+ (gensym "CONTINUE"))
(defvar +ignore+ (gensym "IGNORABLE"))

(defun make-return (val)
  #'(lambda (stream char)
      (declare (ignore stream char))
      val))

(defun hash-table-reader (stream subchar)
  (declare (ignore subchar))
  (let ((vals (loop for token = (read stream NIL NIL T)
                    while (not (eq token +stop+))
                    unless (eq token +ignore+)
                      collect (if (eq 'null token) NIL token))))
    (loop with table = (make-hash-table :test 'equal)
          for (key val) on vals by #'cddr
          do (setf (gethash (deserialize key) table)
                   (deserialize val))
          finally (return table))))

(defun vector-reader (stream subchar)
  (declare (ignore subchar))
  (let ((vals (loop for token = (read stream NIL NIL T)
                    while (not (eq token +stop+))
                    unless (eq token +ignore+)
                      collect (if (eq 'null token) NIL token))))
    (map 'vector #'deserialize vals)))

(defmacro with-json-syntax (() &body body)
  `(let ((*readtable* (copy-readtable))
         (*print-pprint-dispatch* (copy-pprint-dispatch)))
     (set-macro-character #\{ #'hash-table-reader)
     (set-macro-character #\} (make-return +stop+))
     (set-macro-character #\, (make-return +ignore+))
     (set-macro-character #\: (make-return +ignore+))
     (set-macro-character #\[ #'vector-reader)
     (set-macro-character #\] (make-return +stop+))
     (set-pprint-dispatch 'hash-table #'hash-table-printer)
     (set-pprint-dispatch 'string #'string-printer)
     (set-pprint-dispatch 'vector #'vector-printer)
     (set-pprint-dispatch 'symbol #'symbol-printer)
     ,@body))

(define-save-format json (stream object)
  (with-json-syntax ()
    (let ((*serialize-numbers* NIL)
          (*serialize-symbols* NIL)
          (*serialize-hash-tables* T)
          (*level* 0))
      (print (serialize object) stream))))

(define-load-format json (stream)
  (with-json-syntax ()
    (deserialize (read stream))))
