#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config.lisp)

(defun hash-table-printer (stream table)
  (pprint-logical-block (stream () :prefix "{" :suffix "}")
    (format stream "~a" (hash-table-test table))
    (loop for i downfrom (hash-table-count table)
          for k being the hash-keys of table
          for v being the hash-values of table
          do (pprint-newline :mandatory stream)
             (pprint-indent :current 0 stream)
             (format stream "~s ~s" k v)
             (unless (= i 1)
               (write-char #\, stream)))))

(defvar +stop+ (gensym "CONTINUE"))
(defvar +ignore+ (gensym "IGNORABLE"))

(defun make-return (val)
  #'(lambda (stream char)
      (declare (ignore stream char))
      val))

(defun hash-table-reader (stream subchar)
  (declare (ignore subchar))
  (destructuring-bind (test &rest vals) (loop for token = (read stream NIL NIL T)
                                              while (not (eq token +stop+))
                                              unless (eq token +ignore+)
                                                collect token)
    (ecase test (EQ) (EQL) (EQUAL) (EQUALP))
    (loop with table = (make-hash-table :test test)
          for (key val) on vals by #'cddr
          do (setf (gethash (deserialize key) table)
                   (deserialize val))
          finally (return table))))

(defmacro with-hash-table-syntax (() &body body)
  `(let ((*readtable* (copy-readtable))
         (*print-pprint-dispatch* (copy-pprint-dispatch)))
     (set-macro-character #\{ #'hash-table-reader)
     (set-macro-character #\} (make-return +stop+))
     (set-macro-character #\, (make-return +ignore+))
     (set-pprint-dispatch 'hash-table #'hash-table-printer)
     ,@body))

(define-save-format lisp (stream object)
  (with-hash-table-syntax ()
    (let ((*print-array* T)
          (*print-pretty* T)
          (*serialize-symbols* NIL)
          (*serialize-hash-tables* NIL)
          (*serialize-numbers* NIL)
          (*serialize-lists* NIL))
      (print (serialize object) stream))))

(define-load-format lisp (stream)
  (with-hash-table-syntax ()
    (deserialize (read stream))))
