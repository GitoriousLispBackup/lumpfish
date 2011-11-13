;;; Simple proof-of-concept REPL

(in-package #:lumpfish)

(defun repl ()
  (loop
     (format t "~&~a> " (package-name *package*))
     (finish-output)
     (let ((form (gather-complete-form)))
       (when (eq form *eof-object*) (return))
       (handler-case
           (dolist (value (multiple-value-list (eval form)))
             (pprint value))
         (t (e) (format t "~a~%" e))))))

(defun gather-complete-form ()
  (let ((input (make-array '(0)
                           :element-type 'character
                           :fill-pointer t
                           :adjustable t)))
    (loop
       (let ((c (read-char *standard-input* nil nil)))
         (if (eq c nil)
             (return *eof-object*)
             (progn
               (vector-push-extend c input)
               (when (eql c #\Newline)
                 (handler-case
                     (with-input-from-string (s input)
                       (return (lumpfish-read s)))
                   (end-of-file ()
                     (format t "> ")
                     (finish-output))))))))))
