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

(defmacro defexternal (name &optional program)
  `(defun ,name (&rest args)
     (run-external ,(or program (string-downcase (symbol-name name))) args)))

(defexternal git)

(defun run-external (program args)
  (with-output-to-string (output-stream)
    (multiple-value-bind (status code)
        (run program args
             :input nil
             :output output-stream
             :error *error-output*)
      (flet ((err (fmt) (error fmt program args code)))
        (ecase status
          (:exited (when (/= code 0)
                     (err "External command ~a ~a exited with code ~a")))
          (:signaled (err "External command ~a ~a killed by signal ~a")))))))