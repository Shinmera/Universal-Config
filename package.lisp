#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage universal-config
  (:nicknames #:org.tymoonnext.universal-config #:uc)
  (:use #:cl #:parse-float)
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

   #:with-configuration
   #:access
   #:make-container
   #:set-config-tree
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
   #:*fallback-serializer*
   #:*fallback-deserializer*
   #:*serialize-symbols*
   #:*serialize-hash-tables*
   #:*serialize-numbers*
   #:*serialize-lists*
   
   #:escape
   #:unescape
   #:split-escaped
   
   #:serialize
   #:define-string-serializer
   #:define-serializer
   
   #:deserialize
   #:define-string-deserialize
   #:define-deserializer
   #:define-class-de/serializer)
  )

(defpackage universal-config-lisp
  (:nicknames #:org.tymoonnext.universal-config.lisp #:uc-lisp)
  (:use #:cl #:universal-config))

(defpackage universal-config-json
  (:nicknames #:org.tymoonnext.universal-config.json #:uc-json)
  (:use #:cl #:universal-config))
