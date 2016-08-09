(defvar *elr-test/path* (f-dirname (f-this-file)))
(defvar *elr-code/path* (f-parent *elr-test/path*))

(require 'el-reader (f-expand "el-reader.el" *elr-code/path*))

(cl-defun read-complete-buffer (buffer-or-name &optional (read-fn #'read))
  (save-mark-and-excursion
   (let ((exprs))
     (with-current-buffer (get-buffer buffer-or-name)
       (goto-char 1)
       (condition-case nil
           (while t (push (funcall read-fn (current-buffer)) exprs))
         (end-of-file (nreverse exprs)))))))

(defun elr-test/populate-read-objects ()
  (push (vector 1 (cons nil nil) 'error) *el-reader//read-objects*)
  (push (vector 2 (cons nil nil) "foo") *el-reader//read-objects*)
  (push (vector 3 (cons nil nil) t) *el-reader//read-objects*)
  (push (vector 4 (cons nil nil) 5) *el-reader//read-objects*)
  (push (vector 5 (cons nil nil)
                (let ((h (make-hash-table))) (puthash :sample :table h) h))
        *el-reader//read-objects*))

(defmacro elr-test/with-objects (&rest body)
  `(let ((*el-reader//read-objects* nil))
     (elr-test/populate-read-objects)
     ,@body))

(defvar *elr-test/read-decimal-count* 100)
