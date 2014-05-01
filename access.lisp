#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.universal-config)

(defvar *config* (make-hash-table :test 'equal)
  "The global configuration storage variable.
Initialized to a standard EQUAL hash-table.")
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

(defgeneric access (config-object accessor &optional default)
  (:documentation "Universal object accessor.")
  (:method ((o list) accessor &optional default)
    ;; ???
    ;; This is a problem because lists can represent arbitrary data.
    )
  (:method ((o hash-table) accessor &optional default)
    (gethash accessor o default))
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
  (:method (value (o list) accessor)
    ;; ???
    ;; This is a problem because lists can represent arbitrary data.
    )
  (:method (value (o hash-table) accessor)
    (setf (gethash accessor o) value))
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

(defgeneric (setf config-tree) (value &rest accessors)
  (:documentation "Set a value in the configuration.

If *AUGMENT-MISSING-PLACES* is non-NIL, missing path parts
will be attempted to be augmented. The container object is
chosen through MAKE-CONTAINER.")
  (:method (value &rest accessors)
    (loop with last = (car (last accessors))
          for accessor in (butlast accessors)
          for previous = *config* then object
          for (object exists) = (multiple-value-list (access previous accessor))
          do (unless exists
               (unless *augment-missing-places*
                 (error 'inexistent-place :object previous :accessor accessor))
               (warn 'augmenting-place :object previous :accessor accessor)
               (setf object (make-container accessor)
                     (access previous accessor) object))
          finally (setf (access object last) value))))
