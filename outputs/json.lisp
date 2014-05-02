#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.universal-config.json
  (:use #:cl #:universal-config)
  (:nicknames #:universal-config-json #:uc-json))
(in-package #:org.tymoonnext.universal-config.json)

(define-save-format json (stream object)
  (let ((*serialize-numbers* T)
        (*serialize-symbols* T)
        (*serialize-hash-tables* T))
    (yason:encode (serialize object) stream)))

(define-load-format json (stream)
  (deserialize (yason:parse stream :json-arrays-as-vectors T)))
