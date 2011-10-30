(in-package #:lumpfish-reader)

(defvar *lumpfish-readtable* (copy-readtable nil))

(defvar *list-head* nil)

(defun lumpfish-read (stream)
  (loop
     (let ((x (read-char stream)))
       (unless (whitespacep x)
         (let ((macrofun (get-macro-character x *lumpfish-readtable*)))
           (if macrofun
               (let ((result (multiple-value-list
                              (funcall macrofun stream x))))
                 (when result (return-from lumpfish-read (first result))))
               (return (read-token stream x))))))))

(defun read-token (stream first-char)
  (let ((token (make-array '(1)
                           :element-type 'character
                           :initial-element first-char
                           :fill-pointer t
                           :adjustable t)))
    (loop
       (let ((y (read-char stream nil)))
         (when (or (null y) (whitespacep y)) (return))
         (multiple-value-bind (macrofun non-terminating-p)
             (get-macro-character y *lumpfish-readtable*)
           (if (and macrofun (not non-terminating-p))
               (progn
                 (unread-char y stream)
                 (return))
               (vector-push-extend y token)))))
    (multiple-value-bind (integer pos) (parse-integer token :junk-allowed t)
      (cond
        ((= pos (fill-pointer token)) integer)
        ((or *list-head* (eql (char token 0) #\*))
         (get-symbol token))
        (t token)))))

(defun whitespacep (char)
  (some (lambda (c) (eql char c))
        '(#\Linefeed #\Page #\Newline #\Return #\Space #\Tab)))

(defun get-symbol (string)
  (nth-value 0 (intern (ecase (readtable-case *lumpfish-readtable*)
                         (:upcase (string-upcase string))
                         (:downcase (string-downcase string))
                         (:preserve string)))))

(define-condition unmatched-close-paren (error) ())

(defun left-paren-reader (stream char)
  (declare (ignore char))
  (let ((list ()))
    (handler-case
        (progn
          (let ((*list-head* t))
            (push (lumpfish-read stream) list))
          (let ((*list-head* nil))
            (loop (push (lumpfish-read stream) list))))
      (unmatched-close-paren () (nreverse list)))))

(defun right-paren-reader (stream char)
  (declare (ignore stream char))
  (error 'unmatched-close-paren))

(defun single-quote-reader (stream char)
  (declare (ignore char))
  (list 'cl:quote (lumpfish-read stream)))

(defun semicolon-reader (stream char)
  (declare (ignore char))
  (loop
     for c = (read-char stream nil)
     until (or (null c) (eql c #\Newline))))

(defun double-quote-reader (stream char)
  (let ((string (make-array '(0)
                           :element-type 'character
                           :fill-pointer t
                           :adjustable t)))
    (loop
       for c = (read-char stream)
       until (eql c char)
       do (vector-push-extend c string))
    string))

; TODO resolve to (run-external "sh" (list "-c" ...)) ?
(defun backquote-reader (stream char)
  (declare (ignore stream char))
  (error "Backquote syntax not implemented"))

(defun sharpsign-reader (stream char)
  (declare (ignore stream char))
  (error "Sharpsign syntax not implemented"))

(defun dollar-reader (stream char)
  (declare (ignore char))
  (list 'lumpfish-commands:getenv (lumpfish-read stream)))

(set-macro-character #\( #'left-paren-reader nil *lumpfish-readtable*)
(set-macro-character #\) #'right-paren-reader nil *lumpfish-readtable*)
(set-macro-character #\' #'single-quote-reader nil *lumpfish-readtable*)
(set-macro-character #\; #'semicolon-reader nil *lumpfish-readtable*)
(set-macro-character #\" #'double-quote-reader nil *lumpfish-readtable*)
(set-macro-character #\` #'backquote-reader t *lumpfish-readtable*)
(set-macro-character #\# #'sharpsign-reader t *lumpfish-readtable*)

(set-macro-character #\$ #'dollar-reader t *lumpfish-readtable*)