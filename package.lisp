(defpackage #:lumpfish-commands
  (:nicknames #:lf-commands)
  (:use #:cl #:external-program #:osicat)
  (:export #:cd
           #:ec
           #:ls
           #:pwd))

(defpackage #:lumpfish-user
  (:nicknames #:lf-user)
  (:use #:cl #:lumpfish-commands))
