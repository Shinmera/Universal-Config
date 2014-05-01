#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.universal-config.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.universal-config.asdf)

(defsystem universal-config
  :name "Universal-Config"
  :version "2.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Library to provide a universal configuration layer."
  :serial T
  :components ((:file "package")
               (:file "access")
               (:file "transform")
               (:file "output"))
  :depends-on (:cl-ppcre))

;; (defsystem universal-config-doc
;;   :name "Universal-Config Doc"
;;   :components ((:file "documentation"))
;;   :depends-on (:universal-config :lquery-doc))
