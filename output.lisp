#|
 This file is a part of Universal-Config
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *output-format* :lisp
  "The default output format to use.")

(defun save-configuration (path &key (format *output-format*) (object *config*))
  "Save the configuration OBJECT to PATH using the given FORMAT."
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (%save format stream object))
  path)

(defun load-configuration (path &key (format *output-format*) (if-does-not-exist :error))
  "Load the configuration from PATH with the given FORMAT."
  (with-open-file (stream path :direction :input :if-does-not-exist if-does-not-exist)
    (when stream
      (setf *config* (%load format stream)))))

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
