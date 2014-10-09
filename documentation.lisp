#|
 This file is a part of Colleen
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.universal-config.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :universal-config-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.universal-config.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object)))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :universal-config))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :universal-config :exclude '(:internal :method))))
      ($ "#docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :universal-config)))))
