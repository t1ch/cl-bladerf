;;;; cl-bladeRF.asd
;;
;;;; Copyright (c) 2021 Tichaona Kadzinga <tichaona@kadzinga.com>


(asdf:defsystem #:cl-bladerf
  :description "Describe cl-bladeRF here"
  :author "Tichaona Kadzinga <tichaona@kadzinga.com>"
  :license  "GPL Version 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-bladerf")))

(asdf:defsystem #:bladerf-example
  :description "An example usage of cl-bladerf"
  :author "Tichaona Kadzinga <tichaona@kadzinga.com>"
  :license  "GPL Version 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-bladerf #:cl-portaudio)
  :components ((:module "example"
		:serial t
		:components ((:file "package")
			     (:file "bladerf-example")))))
