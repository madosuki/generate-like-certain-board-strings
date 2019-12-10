(defpackage :generate-like-certain-board-strings-asd
  (:use :cl :asdf))

(in-package :generate-like-certain-board-strings-asd)

(defsystem :generate-like-certain-board-strings
  :name "generate-like-certain-board-strings"
  :version "0.01"
  :author "madosuki"
  :licence "Apache 2.0"
  :description "The like certain board strings generator"
  :serial t
  :components ((:file "generate-like-certain-board-strings"))
  :depends-on ("cl-base64" "ironclad" "cl-ppcre" "flexi-streams"))

