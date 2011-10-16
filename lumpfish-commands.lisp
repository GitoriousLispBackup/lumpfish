(in-package #:lumpfish-commands)

(defun cd (&optional (dir (environment-variable "HOME")))
  "Change current directory."
  (setf (current-directory) dir)
  (pwd))

(defun ec (path)
  "Edit a file in emacs."
  (run "emacsclient" (list "-n" path))
  path)

(defun ls (&optional (dir (pwd)))
  "List directory contents."
  (list-directory dir))

(defun pwd ()
  "Pathname of the current directory."
  (current-directory))
