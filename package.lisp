#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage universal-config
  (:nicknames #:org.tymoonnext.universal-config #:uc)
  (:use #:cl)
  ;; access.lisp
  (:export
   #:*config*
   #:*augment-missing-places*
   
   #:inexistent-place
   #:accessor
   #:object
   
   #:augmenting-place
   #:accessor
   #:object
   
   #:access
   #:make-container
   #:config-tree)
  ;; output.lisp
  (:export
   #:*output-format*
   
   #:save-configuration
   #:load-configuration
   
   #:define-save-format
   #:define-load-format)
  ;; transform.lisp
  (:export
   #:escape
   #:unescape
   #:split-escaped
   
   #:serialize
   #:define-string-serializer
   #:define-serializer
   
   #:deserialize
   #:define-string-deserialize
   #:define-deserializer)
  )
