(in-package #:lumpfish-commands)

(defun cd (&optional (dir (getenv "HOME")))
  "Change current directory."
  (setf (current-directory) dir)
  (pwd))

(defun ec (path)
  "Edit a file in emacs."
  (run "emacsclient" (list "-n" path))
  path)

(defun getenv (name)
  (environment-variable name))

(defun (setf getenv) (value name)
  (setf (environment-variable name) value)
  value)

(defun ls (&optional (dir (pwd)))
  "List directory contents."
  (list-directory dir))

(defun pwd ()
  "Pathname of the current directory."
  (current-directory))

(defun whoami ()
  (cdr (assoc :name (user-info (nix:getuid)))))

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
