(require 'cl-lib)
;; (require 'el-reader)

(cl-defun read-complete-buffer (buffer-or-name &optional (read-fn #'read))
  (save-mark-and-excursion
   (let ((exprs))
     (with-current-buffer (get-buffer buffer-or-name)
       (goto-char 1)
       (condition-case nil
           (while t (push (funcall read-fn (current-buffer)) exprs))
         (end-of-file (nreverse exprs)))))))

(progn
  (find-file-noselect "el-reader.el")
  (load "~/code/el-reader/el-reader.elc")
  (garbage-collect)
  (print (benchmark-run (prog1 nil
                          (read-complete-buffer
                           "el-reader.el"
                           #'el-reader/read)))))
