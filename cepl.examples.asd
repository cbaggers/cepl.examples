;;;; cepl.examples.asd

(asdf:defsystem #:cepl.examples
  :description "Examples for the CEPL project"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "GPL V3"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl.sdl2
               #:rtg-math.vari
               #:dendrite #:skitter #:cepl.skitter.sdl2
               #:livesupport #:classimp #:split-sequence
               #:dirt #:temporal-functions)
  :components ((:file "package")
               (:file "examples/helpers/examples-data")
               (:file "examples/helpers/camera")
               (:file "examples/helpers/model-parsers")
               (:file "examples/helpers/meshes")))
