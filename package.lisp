(defpackage #:lumpfish
  (:use #:cl)
  (:export #:*lumpfish-readtable*
           #:lumpfish-read))

(defpackage #:lumpfish-commands
  (:use #:cl #:external-program #:osicat)
  (:export #:cd
           #:defexternal
           #:ec
           #:getenv
           #:git
           #:ls
           #:pwd
           #:whoami))

(defpackage #:lumpfish-user
  (:nicknames #:lf-user)
  (:use #:cl #:lumpfish-commands))
