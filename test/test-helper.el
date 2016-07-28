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

(defvar *elr-test/read-decimal-count* 100)
