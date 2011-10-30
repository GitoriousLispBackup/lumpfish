(defpackage #:lumpfish-commands
  (:nicknames #:lf-commands)
  (:use #:cl #:external-program #:osicat)
  (:export #:cd
           #:defexternal
           #:ec
           #:getenv
           #:ls
           #:pwd
           #:whoami))

(defpackage #:lumpfish-reader
  (:use #:cl)
  (:export #:*lumpfish-readtable* #:lumpfish-read))

(defpackage #:lumpfish-user
  (:nicknames #:lf-user)
  (:use #:cl #:lumpfish-commands))
