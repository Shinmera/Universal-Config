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
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Library to provide a universal configuration layer."
  :serial T
  :components ((:file "package")
               (:file "access")
               (:file "transform")
               (:file "output")
               (:module "outputs"
                :components ((:file "lisp")
                             (:file "json"))))
  :depends-on (:cl-ppcre
               :parse-float))

;; (defsystem universal-config-doc
;;   :name "Universal-Config Doc"
;;   :components ((:file "documentation"))
;;   :depends-on (:universal-config :lquery-doc))
