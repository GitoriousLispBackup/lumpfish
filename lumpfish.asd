(asdf:defsystem #:lumpfish
  :depends-on (#:external-program #:osicat)
  :serial t
  :components ((:file "package")
               (:file "lumpfish-commands")))