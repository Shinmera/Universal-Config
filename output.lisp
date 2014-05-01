#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *output-format* :lisp
  "The default output format to use.")

(defun save-configuration (path &key (format *output-format*) (object *config*))
  "Save the configuration OBJECT to PATH using the given FORMAT."
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (%save format stream (serialize object)))
  path)

(defun load-configuration (path &key (format *output-format*))
  "Load the configuration from PATH with the given FORMAT."
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (setf *config* (deserialize (%load format stream)))))

(defgeneric %save (format path object))

(defgeneric %load (format path))

(defmacro define-save-format (name (streamvar objectvar) &body body)
  "Define a format of NAME to save an object to a stream."
  `(defmethod %save ((,(gensym "FORMAT") (eql ,(intern (string name) "KEYWORD"))) ,streamvar ,objectvar)
     ,@body))

(defmacro define-load-format (name (streamvar) &body body)
  "Define a format of NAME to load an object from a stream."
  `(defmethod %load ((,(gensym "FORMAT") (eql ,(intern (string name) "KEYWORD"))) ,streamvar)
     ,@body))


(defgeneric %print (object stream)
  (:method ((table hash-table) stream)
    (%print (loop with vector = (make-array (* 2 (hash-table-count table)))
                  for i below (/ (array-dimension vector 0) 2)
                  for k being the hash-keys of table
                  for v being the hash-values of table
                  do (setf (aref vector (* 2 i)) k
                           (aref vector (1+ (* 2 i))) v)
                  finally (return vector)) stream))
  (:method ((string string) stream)
    (format stream "~S" string))
  (:method ((vector vector) stream)
    (write-string "#(" stream)
    (loop for item across vector
          for i from 1
          do (%print item stream)
          if (< i (length vector))
            do (write-string " " stream))
    (write-string ")" stream))
  (:method (object stream)
    (format stream "~S" object)))

(define-save-format lisp (stream object)
  (%print object stream))

(define-load-format lisp (stream)
  (read stream))
