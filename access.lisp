#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *config*)
(setf (documentation '*config* 'variable)
      "The global configuration storage variable.")
(defvar *augment-missing-places* T
  "If set to non-NIL, (SETF (CONFIG-TREE ..) ..) will attempt to augment missing places.")

(define-condition inexistent-place (error)
  ((%accessor :initarg :accessor :accessor accessor)
   (%object :initarg :object :accessor object))
  (:documentation "Error condition signalled when attempting to set an inexistent place.")
  (:report (lambda (c s) (format s "Cannot set inexistent place: Accessor ~s does not exist on object ~s."
                                 (accessor c) (object c)))))

(define-condition augmenting-place (warning)
  ((%accessor :initarg :accessor :accessor accessor)
   (%object :initarg :object :accessor object))
  (:documentation "Warning condition signalled when a place is augmented automatically.")
  (:report (lambda (c s) (format s "Augmenting place for accessor ~s on object ~s"
                                 (accessor c) (object c)))))

(defmacro with-configuration ((configuration) &body body)
  "Establishes a configuration context."
  `(let ((*config* ,configuration))
     ,@body))

(defgeneric access (config-object accessor &optional default)
  (:documentation "Universal object accessor.
Returns two values, the first being the accessed value or the supplied default,
the second being T if the requested place was found or NIL if the default is returned.")
  (:method ((o list) (pos fixnum) &optional default)
    (if (< pos (length o))
        (values (nth pos o) T)
        (values default NIL)))
  (:method ((o hash-table) accessor &optional default)
    (gethash accessor o default))
  (:method ((o sequence) (pos fixnum) &optional default)
    (if (< pos (length o))
        (values (elt o pos) T)
        (values default NIL)))
  (:method ((o array) (pos fixnum) &optional default)
    (if (array-in-bounds-p o pos)
        (values (aref o pos) T)
        (values default NIL)))
  (:method ((o array) (scr list) &optional default)
    (if (apply #'array-in-bounds-p o scr)
        (values (apply #'aref o scr) T)
        (values default NIL)))
  (:method ((o standard-object) (slot symbol) &optional default)
    (if (slot-exists-p o slot)
        (values (slot-value o slot) T)
        (values default NIL)))
  (:method ((null null) accessor &optional default)
    (values default NIL)))

(defgeneric (setf access) (value config-object accessor)
  (:documentation "Universal object setter.")
  (:method (value (o list) (pos fixnum))
    (setf (nth pos o) value))
  (:method (value (o hash-table) accessor)
    (setf (gethash accessor o) value))
  (:method (values (o sequence) (pos fixnum))
    (setf (elt o pos) values))
  (:method (value (o array) (pos fixnum))
    (setf (aref o pos) value))
  (:method (value (o array) (scr list))
    ;; Copied from SBCL internals.
    ;; Wouldn't know any other way to setf subscripts at runtime.
    #+sbcl (setf (row-major-aref o (sb-impl::%array-row-major-index o scr))
                 value))
  (:method (value (o standard-object) (slot symbol))
    (setf (slot-value o slot) value))
  (:method (value (null null) accessor)
    (error 'inexistent-place :object null :accessor accessor)))

(defgeneric make-container (accessor)
  (:documentation "Attempts to create a fitting container for an accessor.")
  (:method ((accessor fixnum))
    (make-array accessor :adjustable T :fill-pointer 0))
  (:method ((accessor list))
    (make-array accessor))
  (:method ((accessor symbol))
    (make-hash-table))
  (:method (accessor)
    (make-hash-table :test 'equal)))

(defun config-tree (&rest accessors)
  "Retrieve a value from the configuration."
  (loop for accessor in accessors
        for object = (access *config* accessor)
          then (access object accessor)
        finally (return object)))

(defun set-config-tree (accessors value)
  "Sets the place indicated by the ACCESSORS list to VALUE if possible.
See (SETF CONFIG-TREE)"
  (loop for previous = NIL    then object
        for object = *config* then (access object accessor)
        for prevaccess = NIL  then accessor
        for accessor in accessors
        do (unless object
             (unless *augment-missing-places*
               (error 'inexistent-place :object previous :accessor prevaccess))
             (warn 'augmenting-place :object previous :accessor prevaccess)
             (setf object (make-container accessor)
                   (access previous prevaccess) object))
        finally (setf (access previous accessor) value)))

(defun (setf config-tree) (value &rest accessors)
  "Set a value in the configuration.

If *AUGMENT-MISSING-PLACES* is non-NIL, missing path parts
will be attempted to be augmented. The container object is
chosen through MAKE-CONTAINER."
  (set-config-tree accessors value))
