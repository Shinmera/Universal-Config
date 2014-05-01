#|
 This file is a part of Universal-Config
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.universal-config.json.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.universal-config.json.asdf)

(defsystem universal-config-json
  :name "Universal-Config JSON Format"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Library to provide a universal configuration layer."
  :serial T
  :components ((:file "json"))
  :depends-on (:universal-config
               :yason))
