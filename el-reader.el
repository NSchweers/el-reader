;;;; el-reader.el --- An Advanced reader for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2015 Nathanael Schweers

;; Author: Nathanael Schweers <NSchweers+el-reader@mailbox.org>
;; Created: 09 Nov 2015
;; Keywords: reader
;; Homepage: https://github.com/NSchweers/el-reader

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires ((seq 1.7) (dash 20151021.113) (dash-functional 20150828.413) (emacs 25.0))

;;; Commentary:

;; A flexible reader for elisp.  TODO: Add some more commentary.

;;; Code

;; (define-error 'precond/non-fix "The precondition was not fixed")

;; (defmacro precond (conds &rest body)
;;   "Ensure that preconditions are met before executing BODY.

;; CONDS has the following form ([exp pred fix] ...) where EXP is an
;; expression, which will by bound to `it' in the expansion, via a
;; symbol-macro(!).  This makes it possible to say (setf it foo) in
;; PRED and FIX.  PRED is evaluated once or twice—once to determine
;; whether or not a fix is necessary, and a second time if a fix
;; /is/ necessary.  Should the second evaluation fail, an error is
;; raised.  If it is non-nil, nothing else happens.  If it evaluates
;; to nil, FIX is evaluated once.  In both PRED and FIX, `it'
;; expands to EXP, i.e. it is up to the user to make sure it is
;; evaluated the right number of times."
;;  (declare (indent defun))
;;  `(progn
;;      ,@(cl-loop for (exp pred fix) on conds by #'cdddr
;;                collect
;;                `(cl-symbol-macrolet ((it ,exp))
;;                   (when (not ,pred)
;;                     ,fix
;;                     (when (not ,pred)
;;                       (signal 'precond/non-fix (princ ',pred))))))
;;      ,@body))

(setf lexical-binding t)

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
;; (require 'hash-utils)
(require 'seq)

(eval-when-compile (require 'cl))

(defun ht (&rest args)
  "Create and return a hashtable.

Keys and values are given alternating in args."
  (let ((h (make-hash-table)))
    (cl-loop for (key value) on args by #'cddr
             do (if (and key value) (puthash key value h)
                  (error "Odd number of arguments passed")))
    h))

(cl-define-compiler-macro ht (&rest args)
  "This compiler macro performs loop unrolling.

Unfortunately it does a maximal unroll."
  (cl-labels ((proc-entry (k v h) `(puthash ,k ,v ,h)))
    (if (oddp (length args))
        (error "Odd number of args passed")
      (let ((h (cl-gensym)))
        `(let ((,h (make-hash-table)))
           ,@(cl-loop for (key value) on args by #'cddr
                      collect (proc-entry key value h))
           ,h)))))

(define-error 'end-of-file "End of file reached")

(defclass reader/string-reader-state ()
  ((string :initarg :string :type string)
   (pos :initarg :pos :initform 0 :type integer)))

(defclass reader/function-read-state ()
  ((fn :initarg :function)))

(cl-defgeneric reader/getch (obj &optional char)
  "Read a character using OBJ as source.  If CHAR is given, unread that char.")

(cl-defmethod reader/getch ((b buffer) &optional char)
  "Read or unread a char from a buffer"
  (with-current-buffer b
    (if char
        (if (eq (char-before) char)
            (if (not (bobp))
                (backward-char)
              (error "Unreading to before the buffer begins"))
          (error "Unreading back a different char than was read"))
      (if (eobp)
          (end-of-buffer (signal 'end-of-file "End of buffer reached"))
        (prog1 (char-after)
          (forward-char))))))

(cl-defmethod reader/getch ((m marker) &optional char)
  (with-current-buffer (marker-buffer m)
    (save-mark-and-excursion
     (goto-char m)
     (if char
         (if (eq (char-before) char)
             (if (not (bobp))
                 (decf (marker-position m))
               (error "Unreading to before the buffer begins"))
           (error "Unreading back a different char than was read"))
       (if (eobp)
           (end-of-buffer (signal 'end-of-file "End of buffer reached"))
         (prog1 (char-after)
             (incf (marker-position m))))))))

(cl-defmethod reader/getch ((s reader/string-reader-state) &optional char)
  (with-slots ((s string) pos) s
    (if char
        (progn
          (when (and char (= pos 0)) (error "Unreading to before the string begins"))
          (when (and char (not (= char (elt s (1- pos)))))
            (error "Unreading a different char than was read"))
          (decf pos))
      (if (<= (length s) pos)
          (signal 'end-of-file "End of string")
        (prog1 (elt s pos)
          (incf pos))))))

(cl-defmethod reader/getch ((fn reader/function-read-state) &optional char)
  (funcall (slot-value fn 'fn) char))

(cl-defgeneric reader/get-getch-state (obj)
  "Return an object which is suitable for `reader/getch'.

The default method checks if OBJ is a function, as cl-defgeneric
cannot dispatch on functions.  Otherwise, OBJ is returned as is."
  (if (functionp obj)
      (make-instance 'reader/function-read-state :function obj)
    obj))

(cl-defmethod reader/get-getch-state ((s string))
  (make-instance 'reader/string-reader-state :string s))

(cl-defmethod reader/get-getch-state ((stdin (eql t)))
  (reader/get-getch-state (read-from-minibuffer "Lisp expression: ")))

(cl-defmethod reader/get-getch-state ((stdin (eql nil)))
  (reader/get-getch-state standard-input))

(defclass reader/macro-fn ()
  ((char :initarg :char :type character)
   (fn :initarg :fn)))

;; (defclass reader/readtable ()
;;   ((invalid
;;     :initarg :invalid :initform nil :type list
;;     :documentation
;;     "A list of invalid characters.  Note that there is also a
;;   constituent trait named `invalid'.")
;;    (terminating-macro-chars
;;     :initarg :terminating-macro-chars :initform nil :type list
;;     :documentation
;;     "A list of characters which are terminating macro characters.")
;;    (non-terminating-macro-chars
;;     :initarg :non-terminating-macro-chars :initform nil :type list
;;     :documentation
;;     "")
;;    (whitespace-chars
;;     :initarg :whitespace-chars :initform '(?\s ?\t ?\n ?\e ?\f) :type list
;;     :documentation
;;     "")
;;    (single-escape-chars
;;     :initarg :single-escape-chars :initform nil :type list
;;     :documentation
;;     "")
;;    (multiple-escape-chars
;;     :initarg :multiple-escape-chars :initform nil :type list
;;     :documentation
;;     "")
;;    (constituent-chars
;;     :initarg :constituent-chars :initform nil :type list
;;     :documentation
;;     "")
;;    (traits
;;     :initarg :traits :initform (ht))
;;    (default-syntax-type
;;      :initarg :default-syntax-type :initform 'constituent)
;;    (term-mac-fns
;;     :initarg :term-mac-fns :initform (ht))
;;    (non-term-mac-fns
;;     :initarg :non-term-mac-fns :initform (ht))
;;    (readtable-case
;;     :initarg :readtable-case :initform :preserve)
;;    (char-to-num
;;     :initarg :char-to-num :initform
;;     (ht 48 0 49 1 50 2 51 3 52 4 53 5 54 6 55 7 56 8
;;         57 9 97 10 98 11 99 12 100 13 101 14 102 15
;;         103 16 104 17 105 18 106 19 107 20 108 21 109
;;         22 110 23 111 24 112 25 113 26 114 27 115 28
;;         116 29 117 30 118 31 119 32 120 33 121 34 122
;;         35 65 10 66 11 67 12 68 13 69 14 70 15 71 16
;;         72 17 73 18 74 19 75 20 76 21 77 22 78 23 79
;;         24 80 25 81 26 82 27 83 28 84 29 85 30 86 31
;;         87 32 88 33 89 34 90 35))))

(cl-defstruct (reader/readtable (:conc-name rt/))
  invalid
  terminating-macro-chars
  non-terminating-macro-chars
  (whitespace-chars '(?\s ?\t ?\n ?\e ?\f))
  single-escape-chars
  multiple-escape-chars
  constituent-chars
  (constituent-fn (lambda (_c) t))
  (traits (ht))
  trait-fns
  ;;  (syntax-types (ht))
  (default-syntax-type 'constituent)
  ;; (read-source standard-input)
  (term-mac-fns (ht))
  (non-term-mac-fns (ht))
  (readtable-case :preserve)
  (char-to-num (ht 48 0 49 1 50 2 51 3 52 4 53 5 54 6 55 7 56 8
                   57 9 97 10 98 11 99 12 100 13 101 14 102 15
                   103 16 104 17 105 18 106 19 107 20 108 21 109
                   22 110 23 111 24 112 25 113 26 114 27 115 28
                   116 29 117 30 118 31 119 32 120 33 121 34 122
                   35 65 10 66 11 67 12 68 13 69 14 70 15 71 16
                   72 17 73 18 74 19 75 20 76 21 77 22 78 23 79
                   24 80 25 81 26 82 27 83 28 84 29 85 30 86 31
                   87 32 88 33 89 34 90 35)))

(defclass reader/result ()
  ((success :initarg :success :initform nil :type symbol)
   (token :initarg :token :type string)
   (pos :initarg :pos :type (or integer marker))
   (newpos :initarg :newpos :type (or integer marker null)) ;or marker?
   (result-string :initarg :result-string :type (or string null))
   (rest :initarg :rest :type (or string null))
   (result :initarg :result))
  "A type for the result of a parse.")

(defun reader/result/success-p (r)
  (and (reader/result-p r) (slot-value r 'success)))

(defun reader/make-result (success token pos newpos result-string rest result)
  (make-instance 'reader/result :success success :token token :pos pos :newpos
                 newpos :result-string result-string :rest rest :result result))

(defun reader/make-failed (token pos)
  (make-instance 'reader/result :token token :pos pos :newpos nil :result-string
                 nil :rest nil :result nil))

;; (cl-defstruct (reader/result
;;                (:conc-name reader/result/)
;;                (:constructor reader/make-result (success
;;                                                  token
;;                                                  pos
;;                                                  newpos
;;                                                  result-string
;;                                                  rest
;;                                                  result))
;;                (:constructor reader/make-failed (token pos)))
;;   success
;;   token
;;   pos
;;   newpos
;;   result-string
;;   rest
;;   result)

(defun rt/syntax-type (rt char)
  "Returns a symbol which designates the syntax type of CHAR in RT. "
  (cl-labels
      ((put-syntax-type
          (type)
          (when (cl-member
                 char
                 (funcall (intern (s-concat "rt/" (symbol-name type) "-chars"))
                          *readtable*))
            (if (get-text-property 0 type s)
                (error "More than one syntax type")
              (put-text-property 0 1 'syntax-type (adjust-macro-names type) s)))))
    (cond ((rt/terminating-macro-char-p rt char) 'terminating-macro-char)
          ((rt/non-terminating-macro-char-p rt char) 'non-terminating-macro-char))))

(defun rt//not-non-constituent-p (rt char)
  (let ((l (list #'rt/terminating-macro-chars
                 #'rt/non-terminating-macro-chars
                 #'rt/whitespace-chars
                 #'rt/single-escape-chars
                 #'rt/multiple-escape-chars)))
    (-all? #'null (mapcar (lambda (fn) (member char (funcall fn rt))) l))))


(defun rt/constituentp (rt char)
  "Returns non-nil if CHAR is a constituent char.

This is the case if it is either a member of the slot
CONSTITUENT-CHARS."
  (or (not (null (member char (rt/constituent-chars rt))))
      (funcall (rt/constituent-fn rt) char)))

(defun rt/invalidp (rt char)
  "Return non-nil if char is both constituent and invalid."
  (or (not (null (member char (gethash 'invalid (rt/traits rt)))))
      (-any? (-compose #'not #'null)
             (funcall (apply #'-juxt (rt/trait-fns rt)) char))))

(defun rt/whitespacep (rt char)
  (not (null (member char (rt/whitespace-chars rt)))))

(defun rt/terminating-macro-char-p (rt char)
  (not (null (member char (rt/terminating-macro-chars rt)))))

(defun rt/non-terminating-macro-char-p (rt char)
  (not (null (member char (rt/non-terminating-macro-chars rt)))))

(defun rt/single-escape-char-p (rt char)
  (not (null (member char (rt/single-escape-chars rt)))))

(defun rt/multiple-escape-char-p (rt char)
  (not (null (member char (rt/multiple-escape-chars rt)))))

(defun make-elisp-readtable ()
  (make-reader/readtable
   :whitespace-chars
   '(?\s ?\t ?\n ?\e ?\f)
   ;; We don’t support escaping of token characters yet!  This is not really
   ;; needed for any lisp, other than common lisp.
   ;; We might support the mechanism some day by adding a property to each char,
   ;; which tells us, whether or not it was escaped.
   ;;   :single-escape-chars '(?\\)
   ;;   :multiple-escape-chars '(?|)
   :terminating-macro-chars '(?\" ?' ?\( ?\) ?, ?\; ?`)
   :non-terminating-macro-chars '(?#)
   :constituent-chars '(?\b ?! ?$ ?% ?& ?* ?+ ?- ?. ?/ 48 49 50 51 52 53 54 55
                            56 57 ?: ?< ?= ?> ?? ?@ 65 66 67 68 69 70 71 72 73
                            74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90
                            ?\[ ?\] ?^ ?_ 97 98 99 100 101 102 103 104 105 106
                            107 108 109 110 111 112 113 114 115 116 117 118 119
                            120 121 122 ?{ ?} ?~)
   :traits (ht 'invalid '(?\b ?\t ?\n ?\f ?\r ?\s)
               'alphabetic '(?! ?\" ?# ?$ ?% ?& ? ?\( ?\) ?* ?, ?\; ?< ?= ?> ??
                                ?@ ?[ ?\\ ?] ?^ ?_ ?` ?| ?~ ?{ ?} ?+ ?- ?. ?/)
               'alphadigit '(48 49 50 51 52 53 54 55 56 57 65 66 67 68 69 70 71
                                72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87
                                88 89 90 97 98 99 100 101 102 103 104 105 106
                                107 108 109 110 111 112 113 114 115 116 117 118
                                119 120 121 122)
               'package-marker '(?:)
               'plus-sign '(?+)
               'minus-sign '(?-)
               'dot '(?.)
               'decimal-point '(?.)
               'ratio-marker '(?/)
               'double-float-exponent-marker '(?d ?D)
               'float-exponent-marker '(?e ?E)
               'single-float-exponent-marker '(?f ?F)
               'long-float-exponent-marker '(?l ?L)
               'short-float-exponent-marker '(?s ?S))))

(defvar *readtable*
  (make-elisp-readtable))

(defvar *read-base* 10)

(defvar *reader/preserve-whitespace* nil
  "This variable tells read that it was called as `read-preserving-whitespace'.

Why the Common Lisp folks felt the need to make this into a
separate function instead of an argument is beyond me.

This variable should not be used directly.  It is set by
`read-preserving-whitespace' before calling `read'.")

(defun reader/defaults-to-str-props (char)
  (let ((s (char-to-string char)))
    (cl-labels
        ((adjust-macro-names
          (type)
          (if (string-suffix-p "macro" (symbol-name type))
              (intern (s-concat (symbol-name type) "-char"))
            type))
         (put-syntax-type
          (type)
          (when (cl-member char
                           (funcall
                            (intern (s-concat "rt/" (symbol-name type) "-chars"))
                            *readtable*))
            (if (get-text-property 0 type s)
                (error "More than one syntax type")
              (put-text-property 0 1 'syntax-type (adjust-macro-names type) s))))
         (put-traits
          (type)
          ))
      (--each '(terminating-macro
                non-terminating-macro
                constituent
                whitespace
                single-escape
                multiple-escape)
        (put-syntax-type it))
      (put-text-property
       0 1 'traits 
       (cl-loop for k being the hash-keys in (rt/traits *readtable*)
                if (cl-member char (gethash k (rt/traits *readtable*)))
                collect k)
       s)
      s)))

;;; The reader algorithm is described here:
;;; http://www.lispworks.com/documentation/lw70/CLHS/Body/02_b.htm

;; get-macro-character char &optional readtable => function, non-terminating-p
(cl-defun reader/get-macro-character (char &optional
                                           (readtable *current-readtable*))
  (--if-let (gethash char (rt/term-mac-fns readtable))
      (list it nil)
    (list (gethash char (rt/non-term-mac-fns readtable)) t)))

;; set-macro-character char new-function &optional non-terminating-p readtable => t
(cl-defun reader/set-macro-character (char new-function &optional
                                           non-terminating-p
                                           (readtable *current-readtable*))
  (remhash char (rt/non-term-mac-fns readtable))
  (remhash char (rt/term-mac-fns readtable))
  (if non-terminating-p
      (setf (gethash char (rt/non-term-mac-fns readtable)) new-function)
    (setf (gethash char (rt/term-mac-fns readtable)) new-function))
  t)

(cl-defun reader/put-token-props (token prop val &optional (start (1- (length token)))
                                        (end start))
  (put-text-property start end prop val token)
  token)

(defun reader/token-constituent-p (token pos)
  (get-text-property pos 'syntax-type token))

(defun reader/token-alphabetic-p (token pos)
  (let ((p (get-text-property pos 'traits token)))
    (and (listp p) (eq (car p) 'alphabetic))))

;;; Create classes for each parse result (digit, exponent, etc.)

(defclass reader/syntax-element ()
  ((value :initarg :value)))

(defclass reader/digit (reader/syntax-element)
  ((base :initarg :base :initform 10 :type integer)))

(defclass reader/exponent-marker (reader/syntax-element) ())

(defclass reader/sign (reader/syntax-element) ())

(defclass reader/decimal-point (reader/syntax-element) ())

;; We have a number of functions which combine two parsing functions to create a
;; new one.  Each of them takes two functions which take a token and starting
;; position each.  The resulting function has the same signature.  This way,
;; functions can be chained together.  In types:
;; (token -> pos -> plist) -> (token -> pos -> plist) -> (token -> pos -> plist)

(defun reader/parse-seq (&rest fns)
  (when (null fns)
    (error "At least one argument must be given"))
  (lambda (token pos)
    (cl-block out
      (let ((r (-reduce-from
                (lambda (a f)
                  (if (slot-value a 'success)
                      (with-slots ((a-token token)
                                   (a-newpos newpos)
                                   (a-pos pos)
                                   (a-result result))
                          a
                        (let ((tmp (funcall f a-token
                                            a-newpos)))
                          (with-slots ((tmp-success success)
                                       (tmp-newpos newpos)
                                       (tmp-rest rest)
                                       (tmp-result result))
                              tmp
                            (if tmp-success
                                (reader/make-result
                                 t token pos tmp-newpos
                                 (substring a-token
                                            a-pos
                                            tmp-newpos)
                                 tmp-rest
                                 (cons tmp-result a-result))
                              (reader/make-failed token pos)))))
                    (reader/make-failed token pos)))
                (let ((tmp (funcall (car fns) token pos)))
                  (clone tmp :result (list (slot-value tmp 'result))))
                (cdr fns))))
        (clone r :result (reverse (slot-value r 'result)))))))

(defun reader/parse-alt (&rest fns)
  (when (null fns)
    (error "At least one argument must be given"))
  (lambda (token pos)
    (cl-labels ((helper (fns)
                        (if (null fns)
                            (reader/make-failed token pos)
                          (let ((res (funcall (car fns) token pos)))
                            (if (slot-value res 'success)
                                res
                              (helper (cdr fns)))))))
      (helper fns))))

(defun reader/parse-optional (fn)
  (lambda (token pos)
    (let ((r (funcall fn token pos)))
      (if (slot-value r 'success)
          r
        (reader/make-result t token pos pos "" (substring token pos) nil)))))

(defun reader/parse-kleene-star (fn)
  (lambda (token pos)
    (cl-labels
        ((helper
          (res)
          (let ((tmp (funcall fn token (slot-value res 'newpos))))
            (if (not (slot-value tmp 'success))
                (clone res :result (reverse (slot-value res 'result)))
              (helper
               (reader/make-result t token (slot-value res 'pos)
                                   (slot-value tmp 'newpos)
                                   (substring token (slot-value res 'pos)
                                              (slot-value tmp 'newpos))
                                   (substring token (slot-value tmp 'newpos))
                                   (cons (slot-value tmp 'result)
                                         (slot-value res 'result))))))))
      (let ((res (funcall fn token pos)))
        (if (not (slot-value res 'success))
            res
          (helper (clone res :result (list (slot-value res 'result)))))))))

(defun reader/parse-plus (fn)
  (-compose (lambda (r)
              (if (slot-value r 'success)
                  (clone r :result
                         (cons (car (slot-value r 'result))
                               (cadr (slot-value r 'result))))
                (with-slots (token pos) r
                  (reader/make-failed token pos))))
            (reader/parse-seq fn (reader/parse-kleene-star fn))))

(defun reader/parse-exponent-marker (token pos)
  (if (eq (get-text-property pos 'syntax-type token) 'constituent)
      (-if-let (r (-filter
                   (-compose (-partial (lambda (s1 s2)
                                         (if (string-suffix-p s1 s2) s2))
                                       "exponent-marker")
                             #'symbol-name)
                   (get-text-property pos 'traits token)))
          (if (not (= (length r) 1))
              (error "Ambiguous exponent sign")
            (reader/make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token pos)
                                (make-instance 'reader/exponent-marker
                                               :value (car r))))
        (reader/make-failed token pos))
    (reader/make-failed token pos)))

(defun reader/parse-sign (token pos)
  (if (eq (get-text-property pos 'syntax-type token) 'constituent)
      (-if-let (r (-filter
                   (-compose (-partial (lambda (s1 s2) (if (string-suffix-p s1 s2) s2))
                                       "sign")
                             #'symbol-name)
                   (get-text-property pos 'traits token)))
          (if (not (= (length r) 1))
              (error "Ambiguous sign character")
            (reader/make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token (1+ pos))
                                (make-instance 'reader/sign :value (car r))))
        (reader/make-failed token pos))
    (reader/make-failed token pos)))

(defun reader/parse-decimal-point (token pos)
  (if (or (cl-member 'dot (get-text-property pos 'traits token))
          (cl-member 'decimal-point (get-text-property pos 'traits token)))
      (reader/make-result
       t token pos (1+ pos)
       (substring token pos (1+ pos))
       (substring token (1+ pos))
       (make-instance 'reader/decimal-point
                      :value (substring token pos (1+ pos))))
    (reader/make-failed token pos)))

(defun reader/parse-digit (token pos)
  (if (and (eq (get-text-property pos 'syntax-type token) 'constituent)
           (cl-member 'alphadigit (get-text-property pos 'traits token))
           (cl-member (string-to-char (substring token pos (1+ pos)))
                      (hash-table-keys (rt/char-to-num *readtable*)))
           (< (gethash (string-to-char (substring token pos (1+ pos)))
                       (rt/char-to-num *readtable*))
              *read-base*))
      (reader/make-result t token pos (1+ pos) (substring token pos (1+ pos))
                          (substring token (1+ pos))
                          (make-instance
                           'reader/digit
                           :value (gethash
                                   (string-to-char (substring token pos (1+ pos)))
                                   (rt/char-to-num *readtable*))
                           :base *read-base*))
    (reader/make-failed token pos)))

(defun reader/parse-decimal-digit (token pos)
  (let ((res (reader/parse-digit token pos)))
    (cond ((not (slot-value res 'success))  (reader/make-failed token pos))
          ((not (= (slot-value (slot-value res 'result) 'base) 10))
           (reader/make-failed token pos))
          (t res))))

(defun reader/parse-exponent (token pos)
  (funcall
   (reader/parse-seq #'reader/parse-exponent-marker
                     (reader/parse-optional #'reader/parse-sign)
                     (reader/parse-plus #'reader/parse-digit))
   token pos))

(defun reader/parse-float (token pos)
  (funcall
   (reader/parse-alt (reader/parse-seq
                      (reader/parse-optional #'reader/parse-sign)
                      (reader/parse-kleene-star #'reader/parse-decimal-digit)
                      #'reader/parse-decimal-point
                      (reader/parse-plus #'reader/parse-decimal-digit)
                      (reader/parse-optional #'reader/parse-exponent))
                     (reader/parse-seq
                      (reader/parse-optional #'reader/parse-sign)
                      (reader/parse-plus #'reader/parse-decimal-digit)
                      (reader/parse-optional
                       (reader/parse-seq
                        #'reader/parse-decimal-point
                        (reader/parse-kleene-star
                         #'reader/parse-decimal-digit)))
                      #'reader/parse-exponent))
   token pos))

(defun reader/parse-integer (token pos)
  (let ((int (funcall
              (reader/parse-alt
               (reader/parse-seq
                (reader/parse-optional #'reader/parse-sign)
                (reader/parse-plus #'reader/parse-decimal-digit)
                #'reader/parse-decimal-point)
               (reader/parse-seq
                (reader/parse-optional #'reader/parse-sign)
                (reader/parse-plus #'reader/parse-digit)))
              token pos)))
    (cond ((not (slot-value int 'success)) int)
          ((< (slot-value int 'newpos) (length (slot-value int 'token)))
           (reader/make-failed token pos))
          ((and (listp (slot-value int 'result))
                (-all? #'reader/digit-p (cadr (slot-value int 'result)))
                (-all? (lambda (x) (= (slot-value x 'base)
                                      (slot-value
                                       (caadr (slot-value int 'result))
                                       'base)))
                       (cadr (slot-value int 'result))))
           (let ((base (slot-value (caadr (slot-value int 'result))
                                   'base))
                 (digits (reverse (-map (lambda (x) (slot-value x 'value))
                                        (cadr (slot-value int 'result))))))
             (clone
              int
              :result
              (make-instance
               'reader/syntax-element
               :value (car
                       (--reduce-from
                        (let ((val (car acc))
                              (place (cadr acc)))
                          (list (+ val (* it (expt base place)))
                                (1+ place)))
                        (list (car digits) 1) (cdr digits))))))))))

(defun reader/parse-numeric-token (token pos)
  (funcall
   (reader/parse-alt #'reader/parse-integer
                     #'reader/parse-float)
   token pos))

(defun reader/try-parse-number (token pos)
  (reader/make-result t token pos (length token) token ""
                      (if (and (s-contains? "." (substring token pos))
                               (not (s-suffix? "." (substring token pos))))
                          (string-to-number (substring token pos) 10)
                        (cl-parse-integer
                         token :start pos :end
                         (if (s-suffix? "." (substring token pos))
                             (1- (length token)) (length token))
                         :radix *read-base*))))

;;; In COMMON LISP traits are hardwired to the parser, we should not copy this
;;; mistake.  Also, define a mapping from digits to values.  I.e. make it
;;; possible for *read-base* = 36 to have Z mean (dec 10) and A (dec 35).

(defun reader/process-token (token)
  (reader/parse-float token 0))

(define-error 'reader-error "The reader encountered an error")

;;;###autoload
(cl-defun reader/read (&optional input-stream (eof-error-p t) eof-value
                                 recursive-p)
  (let* ((input-stream (reader/get-getch-state input-stream))
         (x (condition-case c
                (reader/getch input-stream)
              (end-of-file
               (if eof-error-p
                   (signal (car c) (cdr c))
                 (cl-return-from reader/read eof-value))))))
    (cl-labels
        ((has-case-p (c) (not (= (upcase c) (downcase c))))
         (force-alphabetic (c) (let ((z (char-to-string c)))
                                 (put-text-property
                                  0 1 'syntax-type 'constituent z)
                                 (put-text-property
                                  0 1 'traits '(alphabetic) z)
                                 (put-text-property
                                  0 1 'escaped t z)
                                 z))
         (switch-case (c) (if (= (upcase c) c)
                              (downcase c)
                            (upcase c)))
         (step-8 (input-stream token)
                 (let ((y (condition-case c
                              (reader/getch input-stream)
                            (end-of-file (cl-return-from reader/read
                                           (reader/process-token token))))))
                   (cond
                    ((rt/invalidp *readtable* x)
                     (signal 'reader-error (list "Invalid character" x)))
                    ((rt/single-escape-char-p *readtable* y)
                     (let ((z (reader/getch input-stream)))
                       (step-8 input-stream
                               (s-concat token (force-alphabetic z)))))
                    ((rt/multiple-escape-char-p *readtable* y)
                     (step-9 input-stream (reader/defaults-to-str-props x)))
                    ((rt/terminating-macro-char-p *readtable* y)
                     (reader/getch input-stream y)
                     (reader/process-token token))
                    ((rt/whitespacep *readtable* y)
                     (when *reader/preserve-whitespace*
                       (reader/getch input-stream y))
                     (reader/process-token token))
                    ((or (rt/constituentp *readtable* y)
                         (rt/non-terminating-macro-char-p
                          *readtable* y))
                     (step-8 input-stream
                             (s-concat token (reader/defaults-to-str-props y)))))))
         (step-9 (input-stream token)
                 (let ((y (reader/getch input-stream)))
                   (cond
                    ((funcall (-orfn
                               (-map
                                (lambda (fn)
                                  (-partial fn *readtable*))
                                (list
                                 #'rt/constituentp
                                 #'rt/terminating-macro-char-p
                                 #'rt/non-terminating-macro-char-p
                                 #'rt/whitespace-char-p)))
                              y)
                     (step-9
                      input-stream (force-alphabetic y)))
                    ((rt/single-escape-char-p *readtable* y)
                     (step-9
                      input-stream (force-alphabetic
                                    (reader/getch input-stream))))
                    ((rt/multiple-escape-char-p *readtable* y)
                     (step-8 token))
                    ((rt/invalidp *readtable* y)
                     (signal 'reader-error "Invalid char"))))))
      (cond ((rt/invalidp *readtable* x)
             (signal 'reader-error (list "Invalid char" x)))
            ((rt/whitespacep *readtable* x)
             (reader/read input-stream eof-error-p eof-value recursive-p))
            ((or (rt/terminating-macro-char-p *readtable* x)
                 (rt/non-terminating-macro-char-p *readtable* x))

             (--if-let (funcall (gethash x (rt/terminating-macro-chars
                                            *readtable*))
                                input-stream x)
                 (car it)
               (reader/read input-stream eof-error-p eof-value recursive-p)))
            ((rt/single-escape-char-p *readtable* x)
             (signal 'reader-error "Single escape is not yet supported")
             (let ((y (reader/getch input-stream)))
               ;; Treat y as a constituent alphabetic (no other traits!) and begin
               ;; reading a token (step 8).
               (step-8
                input-stream (force-alphabetic y))))
            ((rt/multiple-escape-char-p *readtable* x)
             (signal 'reader-error "Multiple escape is not yet supported")
             (let ((y (reader/getch input-stream)))
               ;; Begin an empty token and proceeed with step 9.
               (step-9 input-stream "")))
            ((rt/constituentp *readtable* x)
             (step-8 input-stream (reader/defaults-to-str-props x)))
            (t (error "PANIC!!! THIS SHOULD NEVER HAVE HAPPENED!!!"))))))

(provide 'el-reader)

;;; el-reader.el ends here
