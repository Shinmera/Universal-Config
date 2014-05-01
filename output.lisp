#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *output-format* :lisp
  "")

(defun save-configuration (path &key (format *output-format*) (object *config*))
  ""
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede :external-format :utf-8)
    (%save format stream (serialize object))))

(defun load-configuaration (path &key (format *output-format*))
  ""
  (with-open-file (stream path :direction :input :if-does-not-exist :error :external-format :utf-8)
    (setf *config* (deserialize (%load format path)))))

(defgeneric %save (format path object))

(defgeneric %load (format path))

(defmacro define-save-format (name (streamvar objectvar) &body body)
  ""
  `(defmethod %save ((,(gensym "FORMAT") (eql ,(intern (string name) "KEYWORD"))) ,streamvar ,objectvar)
     ,@body))

(defmacro define-load-format (name (streamvar) &body body)
  ""
  `(defmethod %load ((,(gensym "FORMAT") (eql ,(intern (string name) "KEYWORD"))) ,streamvar)
     ,@body))

(define-save-format lisp (stream object)
  (print object stream))

(define-load-format lisp (stream)
  (read stream))
