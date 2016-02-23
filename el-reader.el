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
;; The reader algorithm is described here:
;; http://www.lispworks.com/documentation/lw70/CLHS/Body/02_b.htm

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
;;;Emacs (especially org-mode) is sometimes a little eratic when it comes to
;;;variables defined with -*- syntax (see first line).  This is why `setf' is
;;;used here.

(require 'cl-lib)
(require 'eieio)
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

(defclass el-reader/string-reader-state ()
  ((string :initarg :string :type string)
   (pos :initarg :pos :initform 0 :type integer)))

(defclass el-reader/function-read-state ()
  ((fn :initarg :function)))

(cl-defgeneric el-reader/getch (stream &optional char)
  "Read a character using OBJ as source.  If CHAR is given, unread that char.")

;; (cl-defun el-reader/read-char (&optional (stream standard-input)
;;                                          (eof-error-p t)
;;                                          eof-value
;;                                          _recursive-p)
;;   (condition-case c
;;       (el-reader/getch stream)
;;     (cl-no-applicable-method
;;      (el-reader/read-char (el-reader/get-getch-state stream) eof-error-p
;;                           eof-value _recursive-p))
;;     (end-of-file (if eof-error-p
;;                      (signal (car c) (cdr c))
;;                    eof-value))))

(cl-defun el-reader/peek-char (stream)
  (let ((c (el-reader/getch stream)))
    (el-reader/getch stream c)
    c))

(cl-defmethod el-reader/getch ((b buffer) &optional char)
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

(cl-defmethod el-reader/getch ((m marker) &optional char)
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

(cl-defmethod el-reader/getch ((s el-reader/string-reader-state) &optional char)
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

(cl-defmethod el-reader/getch ((fn el-reader/function-read-state) &optional char)
  (funcall (slot-value fn 'fn) char))

(cl-defgeneric el-reader/get-getch-state (obj)
  "Return an object which is suitable for `el-reader/getch'.

The default method checks if OBJ is a function, as cl-defgeneric
cannot dispatch on functions.  Otherwise, OBJ is returned as is."
  (if (functionp obj)
      (make-instance 'el-reader/function-read-state :function obj)
    obj))

(cl-defmethod el-reader/get-getch-state ((s string))
  (make-instance 'el-reader/string-reader-state :string s))

(cl-defmethod el-reader/get-getch-state ((stdin (eql t)))
  (el-reader/get-getch-state (read-from-minibuffer "Lisp expression: ")))

(cl-defmethod el-reader/get-getch-state ((stdin (eql nil)))
  (el-reader/get-getch-state standard-input))

(defclass el-reader/macro-fn ()
  ((char :initarg :char :type character)
   (fn :initarg :fn)))

;;; In COMMON LISP traits are hardwired to the parser, we should not copy this
;;; mistake.  Also, define a mapping from digits to values.  I.e. make it
;;; possible for *read-base* = 36 to have Z mean (dec 10) and A (dec 35).

(defclass el-reader/readtable ()
  ((invalid-chars :initarg :invalid :initform nil :type list)
   ;; Include as many whitespace chars as possible.  Here we have the regular
   ;; space (?\s -- 32), nobreak space (?\  -- 160) and thin space (?\  --
   ;; 8239).
   (whitespace-chars :initarg :whitespace-chars
                     :initform '(?\s ?\t ?\n ?\e ?\f ?\  ?\ )
                     :type list)
   (single-escape-chars :initarg :single-escape-chars :initform nil :type list)
   (multiple-escape-chars :initarg :multiple-escape-chars :initform nil
                          :type list)
   (constituent-chars :initarg :constituent-chars :initform nil :type list)
   (traits :initarg :traits :initform (ht) :type hash-table)
   (term-mac-fns :initarg :term-mac-fns :initform (ht) :type hash-table)
   (non-term-mac-fns :initarg :non-term-mac-fns :initform (ht) :type hash-table)
   (readtable-case :initarg :readtable-case :initform :preserve)
   (char-to-num :initarg :char-to-num :initform
                (ht 48 0 49 1 50 2 51 3 52 4 53 5 54 6 55 7 56 8
                    57 9 97 10 98 11 99 12 100 13 101 14 102 15
                    103 16 104 17 105 18 106 19 107 20 108 21 109
                    22 110 23 111 24 112 25 113 26 114 27 115 28
                    116 29 117 30 118 31 119 32 120 33 121 34 122
                    35 65 10 66 11 67 12 68 13 69 14 70 15 71 16
                    72 17 73 18 74 19 75 20 76 21 77 22 78 23 79
                    24 80 25 81 26 82 27 83 28 84 29 85 30 86 31
                    87 32 88 33 89 34 90 35))))

(defmacro el-reader/make-public-accessors (class &optional prefix)
  (cons 'progn
        (seq-map
         (lambda (e)
           `(progn
              (defun ,(intern (s-concat (if prefix (symbol-name prefix) "")
                                        (symbol-name e)))
                  (obj)
                ,(s-concat "Extract the element " (symbol-name e) " from OBJ. ")
                (slot-value obj ',e))
              (cl-defmethod (setf ,(intern (s-concat
                                            (if prefix
                                                (symbol-name prefix)
                                              "")
                                            (symbol-name e))))
                  (new-elt (rt el-reader/readtable))
                (setf (slot-value obj ',e) new-elt))))
         (object-slots (make-instance class)))))

(el-reader/make-public-accessors el-reader/readtable el-reader/rt/)

;; get-macro-character char &optional readtable => function, non-terminating-p
(cl-defun el-reader/get-macro-character (char &optional
                                           (readtable *readtable*))
  (--if-let (gethash char (el-reader/rt/term-mac-fns readtable))
      (list it nil)
    (list (gethash char (el-reader/rt/non-term-mac-fns readtable)) t)))

;; set-macro-character char new-function &optional non-terminating-p readtable => t
(cl-defun el-reader/set-macro-character (char new-function &optional
                                           non-terminating-p
                                           (readtable *readtable*))
  (remhash char (el-reader/rt/non-term-mac-fns readtable))
  (remhash char (el-reader/rt/term-mac-fns readtable))
  (when new-function
    (if non-terminating-p
        (setf (gethash char (el-reader/rt/non-term-mac-fns readtable))
              new-function)
      (setf (gethash char (el-reader/rt/term-mac-fns readtable))
            new-function))
    t))

(defclass el-reader/result ()
  ((success :initarg :success :initform nil :type symbol)
   (token :initarg :token :type string)
   (pos :initarg :pos :type (or integer marker))
   (newpos :initarg :newpos :type (or integer marker null)) ;or marker?
   (result-string :initarg :result-string :type (or string null))
   (rest :initarg :rest :type (or string null))
   (result :initarg :result))
  "A type for the result of a parse.")

(defun el-reader/result/success-p (r)
  (and (el-reader/result-p r) (slot-value r 'success)))

(defun el-reader/make-result (success token pos newpos
                                      result-string rest result)
  (make-instance 'el-reader/result :success success :token token :pos
                 pos :newpos newpos :result-string result-string :rest rest
                 :result result))

(defun el-reader/make-failed (token pos)
  (make-instance 'el-reader/result :token token :pos pos :newpos nil
                 :result-string nil :rest nil :result nil))

(defun el-reader/rt/constituentp (rt char)
  "Returns non-nil if CHAR is a constituent char.

This is the case if it is either a member of the slot
CONSTITUENT-CHARS, or not a member of the other classes"
  (or (not (null (member char (el-reader/rt/constituent-chars rt))))
      (not
       (funcall
        (apply #'-orfn
               (seq-map
                (lambda (f) (-partial f rt))
                (list  #'el-reader/rt/invalidp
                      #'el-reader/rt/terminating-macro-char-p
                      #'el-reader/rt/non-terminating-macro-char-p
                      #'el-reader/rt/whitespacep
                      #'el-reader/rt/single-escape-char-p
                      #'el-reader/rt/multiple-escape-char-p)))
        char))))

(defun el-reader/rt/invalid-syntax-type-p (rt char)
  "Return non-nil if CHAR is of syntax type invalid."
  (when (member char (el-reader/rt/invalid-chars rt))
    t))

(defun el-reader/rt/invalid-trait-p (rt char)
  "Return non-nil if CHAR has the invalid trait.

Note that this may wrongly return non-nil, if a trait was set for
a non-constituent character."
  (when (member char (gethash 'invalid (el-reader/rt/traits rt)))
    t))

(defun el-reader/rt/invalidp (rt char)
  "Return non-nil if CHAR is either both constituent and invalid, or of
syntax-type invalid."
  (or (el-reader/rt/invalid-syntax-type-p rt char)
      (el-reader/rt/invalid-trait-p rt char)))

(defun el-reader/rt/whitespacep (rt char)
  (not (null (member char (el-reader/rt/whitespace-chars rt)))))

(defun el-reader/rt/terminating-macro-char-p (rt char)
  (not (null (member char (cl-loop for k being the hash-keys in
                                   (slot-value rt 'term-mac-fns)
                                   collect k)))))

(defun el-reader/rt/non-terminating-macro-char-p (rt char)
  (not (null (member char (cl-loop for k being the hash-keys in
                                   (slot-value rt 'non-term-mac-fns)
                                   collect k)))))

(defun el-reader/rt/single-escape-char-p (rt char)
  (not (null (member char (el-reader/rt/single-escape-chars rt)))))

(defun el-reader/rt/multiple-escape-char-p (rt char)
  (not (null (member char (el-reader/rt/multiple-escape-chars rt)))))

;;; Define read macros here.

(defun el-reader/read-string (stream char)
  (el-reader/getch stream char)
  (read (-partial #'el-reader/getch stream)))

(defun el-reader/read-char (stream char)
  (let* ((ch1 (el-reader/getch stream))
         (str (s-concat "?" (char-to-string ch1))))
    (when (= ch1 ?\\)
      (setf str (s-concat str (char-to-string (el-reader/getch stream)))))
    (read str)))

(defun make-elisp-readtable ()
  (make-instance 'el-reader/readtable
   :whitespace-chars
   '(?\s ?\t ?\n ?\e ?\f)
   ;; We don’t support escaping of token characters yet!  This is not really
   ;; needed for any lisp, other than common lisp.
   ;; We might support the mechanism some day by adding a property to each char,
   ;; which tells us, whether or not it was escaped.
   ;;   :single-escape-chars '(?\\)
   ;;   :multiple-escape-chars '(?|)
   ;; :terminating-macro-chars '(?\" ?' ?\( ?\) ?, ?\; ?` ??)
   ;; :non-terminating-macro-chars '(?#)
   :constituent-chars '(?\b ?! ?$ ?% ?& ?* ?+ ?- ?. ?/ 48 49 50 51 52 53 54 55
                            56 57 ?: ?< ?= ?> ?? ?@ 65 66 67 68 69 70 71 72 73
                            74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90
                            ?\[ ?\] ?^ ?_ 97 98 99 100 101 102 103 104 105 106
                            107 108 109 110 111 112 113 114 115 116 117 118 119
                            120 121 122 ?{ ?} ?~)
   :traits (ht 'invalid '(?\b ?\t ?\n ?\f ?\r ?\s)
               'alphabetic '(?! ?\" ?# ?$ ?% ?& ? ?\( ?\) ?* ?, ?\; ?< ?= ?> ;; ??
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

(defvar *el-reader/preserve-whitespace* nil
  "This variable tells read that it was called as `read-preserving-whitespace'.

Why the Common Lisp folks felt the need to make this into a
separate function instead of an argument is beyond me.

This variable should not be used directly.  It is set by
`read-preserving-whitespace' before calling `read'.")

(defvar *el-reader//allow-single-dot-symbol* nil
  "Whether to allow a symbol named `.'.
Do not use this variable!  It is used internally in the reader.
Only a function called by `read' and the list reading code use
it.  It shall stay that way!")

(defun el-reader/defaults-to-str-props (char)
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
                            (intern (s-concat "el-reader/rt/"
                                              (symbol-name type) "-chars"))
                            *readtable*))
            (if (get-text-property 0 type s)
                (error "More than one syntax type")
              (put-text-property 0 1 'syntax-type
                                 (adjust-macro-names type) s)))))
      (seq-do
       (lambda (sym) (put-syntax-type sym))
       '(terminating-macro
         non-terminating-macro
         constituent
         whitespace
         single-escape
         multiple-escape))
      (put-text-property
       0 1 'traits 
       (cl-loop for k being the hash-keys in (el-reader/rt/traits *readtable*)
                if (cl-member char (gethash k (el-reader/rt/traits
                                               *readtable*)))
                collect k)
       s)
      s)))

(defun el-reader/defaults-to-str-props (char)
  (let ((s (char-to-string char)))
    (cl-labels
        ((put-syntax-type
          (type)
          (when (funcall
                 (intern
                  (s-concat
                   "el-reader/rt/"
                   (symbol-name type)
                   (if (string-match-p "-" (symbol-name type))
                       "-p"
                     "p")))
                 *readtable* char)
            (if (get-text-property 0 type s)
                (error "More than one syntax type")
              (put-text-property 0 1 'syntax-type
                                 type s)))))
      (seq-do
       (lambda (sym) (put-syntax-type sym))
       '(terminating-macro-char
         non-terminating-macro-char
         constituent
         whitespace
         single-escape-char
         multiple-escape-char))
      (put-text-property
       0 1 'traits
       (cl-loop for k being the hash-keys in (el-reader/rt/traits *readtable*)
                if (cl-member char (gethash k (el-reader/rt/traits
                                               *readtable*)))
                collect k)
       s)
      s)))

(cl-defun el-reader/put-token-props (token prop val &optional
                                           (start (1- (length token)))
                                           (end start))
  (put-text-property start end prop val token)
  token)

(defun el-reader/token-constituent-p (token pos)
  (get-text-property pos 'syntax-type token))

(defun el-reader/token-alphabetic-p (token pos)
  (let ((p (get-text-property pos 'traits token)))
    (and (listp p) (eq (car p) 'alphabetic))))

;;; This is a recursive descent parser for number types.  This may be stripped
;;; away in a future version.  The main reason to strip it, is that Emacs’
;;; default reader may be used.  The main reason to keep it, is that this
;;; version may be easier to extend.  For now it stays.

;;; Create classes for each parse result (digit, exponent, etc.)

(defclass el-reader/syntax-element ()
  ((value :initarg :value)))

(defclass el-reader/digit (el-reader/syntax-element)
  ((base :initarg :base :initform 10 :type integer)))

(defclass el-reader/exponent-marker (el-reader/syntax-element) ())

(defclass el-reader/sign (el-reader/syntax-element) ())

(defclass el-reader/decimal-point (el-reader/syntax-element) ())

;; We have a number of functions which combine two parsing functions to create a
;; new one.  Each of them takes two functions which take a token and starting
;; position each.  The resulting function has the same signature.  This way,
;; functions can be chained together.  In types:
;; (token -> pos -> plist) -> (token -> pos -> plist) -> (token -> pos -> plist)

(defun el-reader/ensure-complete-token (fn)
  (lambda (token pos)
    (let ((res (funcall fn token pos)))
      (cond ((and (slot-value res 'success)
                  (or (not (string-empty-p (slot-value res 'rest)))
                      (< (slot-value res 'newpos) (length token))))
             (el-reader/make-failed token pos))
            (t res)))))

(defun el-reader/parse-seq (&rest fns)
  (when (null fns)
    (error "At least one argument must be given"))
  (lambda (token pos)
    (let ((r (seq-reduce
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
                              (el-reader/make-result
                               t token pos tmp-newpos
                               (substring a-token
                                          a-pos
                                          tmp-newpos)
                               tmp-rest
                               (cons tmp-result a-result))
                            (el-reader/make-failed token pos)))))
                  (el-reader/make-failed token pos)))
              (cdr fns)
              (let ((tmp (funcall (car fns) token pos)))
                (clone tmp :result (list (slot-value tmp 'result)))))))
      (clone r :result (reverse (slot-value r 'result))))))

(defun el-reader/parse-alt (&rest fns)
  (when (null fns)
    (error "At least one argument must be given"))
  (lambda (token pos)
    (cl-labels ((helper (fns)
                        (if (null fns)
                            (el-reader/make-failed token pos)
                          (let ((res (funcall (car fns) token pos)))
                            (if (slot-value res 'success)
                                res
                              (helper (cdr fns)))))))
      (helper fns))))

(defun el-reader/parse-optional (fn)
  (lambda (token pos)
    (let ((r (funcall fn token pos)))
      (if (slot-value r 'success)
          r
        (el-reader/make-result t token pos pos "" (substring token pos) nil)))))

(defun el-reader/parse-kleene-star (fn)
  (lambda (token pos)
    (cl-labels
        ((helper
          (res)
          (let ((tmp (funcall fn token (slot-value res 'newpos))))
            (if (not (slot-value tmp 'success))
                (clone res :result (reverse (slot-value res 'result)))
              (helper
               (el-reader/make-result t token (slot-value res 'pos)
                                   (slot-value tmp 'newpos)
                                   (substring token (slot-value res 'pos)
                                              (slot-value tmp 'newpos))
                                   (substring token (slot-value tmp 'newpos))
                                   (cons (slot-value tmp 'result)
                                         (slot-value res 'result))))))))
      (let ((res (funcall fn token pos)))
        (if (not (slot-value res 'success))
            (el-reader/make-result t token pos pos "" (substring token pos) nil)
          (helper (clone res :result (list (slot-value res 'result)))))))))

(defun el-reader/parse-plus (fn)
  (-compose (lambda (r)
              (if (slot-value r 'success)
                  (clone r :result
                         (cons (car (slot-value r 'result))
                               (cadr (slot-value r 'result))))
                (with-slots (token pos) r
                  (el-reader/make-failed token pos))))
            (el-reader/parse-seq fn (el-reader/parse-kleene-star fn))))

(defun el-reader/parse-exponent-marker (token pos)
  (if (eq (get-text-property pos 'syntax-type token) 'constituent)
      (-if-let (r (seq-filter
                   (-compose (-partial (lambda (s1 s2)
                                         (if (string-suffix-p s1 s2) s2))
                                       "exponent-marker")
                             #'symbol-name)
                   (get-text-property pos 'traits token)))
          (if (not (= (length r) 1))
              (error "Ambiguous exponent sign")
            (el-reader/make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token pos)
                                (make-instance 'el-reader/exponent-marker
                                               :value (car r))))
        (el-reader/make-failed token pos))
    (el-reader/make-failed token pos)))

(defun el-reader/parse-sign (token pos)
  (if (eq (get-text-property pos 'syntax-type token) 'constituent)
      (-if-let (r (seq-filter
                   (-compose (-partial
                              (lambda (s1 s2) (if (string-suffix-p s1 s2) s2))
                              "sign")
                             #'symbol-name)
                   (get-text-property pos 'traits token)))
          (if (not (= (length r) 1))
              (error "Ambiguous sign character")
            (el-reader/make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token (1+ pos))
                                (make-instance 'el-reader/sign :value (car r))))
        (el-reader/make-failed token pos))
    (el-reader/make-failed token pos)))

(defun el-reader/parse-decimal-point (token pos)
  (if (or (cl-member 'dot (get-text-property pos 'traits token))
          (cl-member 'decimal-point (get-text-property pos 'traits token)))
      (el-reader/make-result
       t token pos (1+ pos)
       (substring token pos (1+ pos))
       (substring token (1+ pos))
       (make-instance 'el-reader/decimal-point
                      :value (substring token pos (1+ pos))))
    (el-reader/make-failed token pos)))

(defun el-reader/parse-digit (token pos)
  (if (and (eq (get-text-property pos 'syntax-type token) 'constituent)
           (cl-member 'alphadigit (get-text-property pos 'traits token))
           (cl-member (string-to-char (substring token pos (1+ pos)))
                      (hash-table-keys (el-reader/rt/char-to-num *readtable*)))
           (< (gethash (string-to-char (substring token pos (1+ pos)))
                       (el-reader/rt/char-to-num *readtable*))
              *read-base*))
      (el-reader/make-result t token pos (1+ pos) (substring token pos (1+ pos))
                          (substring token (1+ pos))
                          (make-instance
                           'el-reader/digit
                           :value (gethash
                                   (string-to-char
                                    (substring token pos (1+ pos)))
                                   (el-reader/rt/char-to-num *readtable*))
                           :base *read-base*))
    (el-reader/make-failed token pos)))

(defun el-reader/parse-decimal-digit (token pos)
  (let ((res (el-reader/parse-digit token pos)))
    (cond ((not (slot-value res 'success))  (el-reader/make-failed token pos))
          ((not (= (slot-value (slot-value res 'result) 'base) 10))
           (el-reader/make-failed token pos))
          (t res))))

(defun el-reader/parse-exponent (token pos)
  (funcall
   (el-reader/parse-seq #'el-reader/parse-exponent-marker
                     (el-reader/parse-optional #'el-reader/parse-sign)
                     (el-reader/parse-plus #'el-reader/parse-digit))
   token pos))

(defun el-reader/same-base-p (digits)
  (let ((first-base (slot-value (car digits) 'base)))
    (seq-every-p (lambda (it) (= first-base (slot-value it 'base)))
                 (cdr digits))))

(defun el-reader/proper-float-p (float)
  (let ((res (slot-value float 'result)))
    (and (seq-every-p #'el-reader/digit-p (second res))
         (= (slot-value (second res) 'base) 10)
         (el-reader/same-base-p (append (second res)
                                        (fourth res))))))

(defun el-reader/adjust-for-sign (num sign)
  (let ((sign-fn (if (and (el-reader/sign-p sign)
                          (eq (slot-value sign 'value)
                              'minus-sign))
                     #'- #'identity)))
    (funcall sign-fn num)))

(defun el-reader/digit-list->int (digits)
  (when (and (not (null digits)) (listp digits) (el-reader/same-base-p digits))
    (let ((base (slot-value (car digits) 'base))
          (digits (reverse (seq-map (lambda (x) (slot-value x 'value))
                                    digits))))
      (car (seq-reduce
            (lambda (acc it)
              (let ((val (car acc))
                    (place (cadr acc)))
                (list (+ val (* it (expt base place)))
                      (1+ place))))
            (cdr digits) (list (car digits) 1))))))

(defun el-reader/make-int (sign-and-digits)
  (el-reader/adjust-for-sign
   (el-reader/digit-list->int
    (cadr sign-and-digits))
   (car sign-and-digits)))

(defun el-reader/drop-trailing-zeros (digits)
  (reverse (el-reader/drop-leading-zeros (reverse digits))))

(defun el-reader/drop-leading-zeros (digits)
  "DIGITS is a seq of digit objects (`el-reader/digit').
Leading zeros are dropped, the rest is returned as is."
  (seq-drop-while (lambda (d) (zerop (slot-value d 'value)))
                  digits))

(defun el-reader/drop-trailing-zeros (digits)
  "See `el-reader/drop-leading-zeros'."
  (seq-reverse (el-reader/drop-leading-zeros (seq-reverse digits))))

(defun el-reader/parse-float (token pos)
  (let* ((flt (funcall
               (el-reader/ensure-complete-token
                (el-reader/parse-alt
                 (el-reader/parse-seq
                  (el-reader/parse-optional #'el-reader/parse-sign)
                  (el-reader/parse-kleene-star #'el-reader/parse-decimal-digit)
                  #'el-reader/parse-decimal-point
                  (el-reader/parse-plus #'el-reader/parse-decimal-digit)
                  (el-reader/parse-optional #'el-reader/parse-exponent))
                 (el-reader/parse-seq
                  (el-reader/parse-optional #'el-reader/parse-sign)
                  (el-reader/parse-plus #'el-reader/parse-decimal-digit)
                  (el-reader/parse-optional
                   (el-reader/parse-seq
                    #'el-reader/parse-decimal-point
                    (el-reader/parse-kleene-star
                     #'el-reader/parse-decimal-digit)))
                  #'el-reader/parse-exponent)))
               token pos))
         (res (slot-value flt 'result)))
    ;;; NB: “Left” refers to the grammar given here:
    ;;; http://www.lispworks.com/documentation/lw70/CLHS/Body/02_ca.htm Left
    ;;; mereley means that it is the first case, rather than the latter.  This
    ;;; is why the extraction of elements of the parse tree is slightly
    ;;; different for the two cases.
    (cl-labels
        ((get-sign-fn (num)
                      (let ((sign (car num)))
                        (if (and (el-reader/sign-p sign)
                                 (eq (slot-value sign 'value) 'minus-sign))
                            #'-
                          #'identity)))
         (get-int-part
          (dec-digits)
          (el-reader/digit-list->int (el-reader/drop-leading-zeros dec-digits)))
         (get-post-dec-places
          (mantissa)
          (let* ((digits (el-reader/drop-trailing-zeros mantissa))
                 (digit-count (length digits))
                 (int (or (get-int-part digits) 0.0)))
            (* int (expt 10 (- digit-count)))))
         (fe (float)
             (let* ((left (not (or (null (third float)) (and (listp (third float))
                                      (= (length (third float)) 2)))))
                    (dec-digits (cadr float))
                    (dec-point? (if left
                                    (third float)
                                  (car (third float))))
                    (mantissa (if left (fourth float)
                                (second (third float))))
                    (exponent (if left (fifth float) (fourth float))))
               (* (funcall
                   (get-sign-fn float)
                   (+ (get-int-part dec-digits)
                      (get-post-dec-places mantissa)))
                  (if exponent
                      (expt 10 (funcall (get-sign-fn (list (second exponent)))
                                        (get-int-part (third exponent))))
                    1.0)))))
      (if (slot-value flt 'success)
          (clone
           flt
           :result
           (make-instance
            'el-reader/syntax-element
            :value (fe (slot-value flt 'result))))
        flt))))

(defun el-reader/parse-integer (token pos)
  (let ((int (funcall
              (el-reader/ensure-complete-token
               (el-reader/parse-alt
                (el-reader/parse-seq
                 (el-reader/parse-optional #'el-reader/parse-sign)
                 (el-reader/parse-plus #'el-reader/parse-decimal-digit)
                 #'el-reader/parse-decimal-point)
                (el-reader/parse-seq
                 (el-reader/parse-optional #'el-reader/parse-sign)
                 (el-reader/parse-plus #'el-reader/parse-digit))))
              token pos)))
    (if (slot-value int 'success)
        (clone
         int
         :result
         (make-instance
          'el-reader/syntax-element
          :value (el-reader/make-int (slot-value int 'result))))
      int)))

(defun el-reader/parse-numeric-token (token pos)
  (funcall
   (el-reader/parse-alt #'el-reader/parse-integer
                        #'el-reader/parse-float)
   token pos))

(defun el-reader//map-string-as-substrings (fn token)
  "Works like `mapcar' on a string, yet passes the chars as singleton strings,
leaving the properties intact.  The result is a list of the results, in order."
  (let ((res nil))
    (dotimes (i (length token))
      (push (funcall fn (substring token i (1+ i)))
            res))
    (seq-reverse res)))

(define-error 'reader-error "The reader encountered an error")

(defun el-reader/parse-symbol (token pos)
  (when (not (zerop pos))
    (warn "POS is nonzero, this probably shouldn’t happen!"))
  (cl-labels ((escaped-p
               (str pos)
               (get-text-property pos 'escapedp str)))
    (let ( (name (substring-no-properties token pos)))
      (if (and (string= name ".")
               (not (get-text-property pos 'escapedp token))
               (not *el-reader//allow-single-dot-symbol*))
          (signal 'reader-error "invalid-read-syntax: \".\"")
        (intern name)))))
;;; The parser is now defined and may be used.

;TODO: possibly build in package support.  This would need a hook of some sort.
(defun el-reader/process-token (token)
  (let ((num? (el-reader/parse-numeric-token token 0)))
    (if (slot-value num? 'success)
        (slot-value (slot-value num? 'result) 'value)
      (el-reader/parse-symbol token 0))))

(defun el-reader/read-delimited-list (char &optional stream _recursive-p)
  (let ((res-list nil)
        (c (el-reader/peek-char stream)))
    (while (/= c char)
      (cond ((el-reader/rt/invalidp *readtable* c)
             (signal 'reader-error (list "Invalid char" c)))
            ((el-reader/rt/whitespacep *readtable* c)
             (el-reader/getch stream)
             (setf c (el-reader/peek-char stream)))
            ((or (el-reader/rt/constituentp *readtable* c)
                 (el-reader/rt/terminating-macro-char-p *readtable* c)
                 (el-reader/rt/non-terminating-macro-char-p *readtable* c)
                 (el-reader/rt/single-escape-char-p *readtable* c)
                 (el-reader/rt/multiple-escape-char-p *readtable* c))
             (push (el-reader/read stream t nil t) res-list)
             (setf c (el-reader/peek-char stream)))))
    (el-reader/getch stream)
    (reverse res-list)))

(defun el-reader/read-lisp-list (stream char)
  (let ((*el-reader//allow-single-dot-symbol* t))
    (let ((l (el-reader/read-delimited-list ?\) stream t)))
      (let ((l- (if (and (>= (seq-length l) 3)
                         (eq (seq-elt l (- (length l) 2)) (intern ".")))
                    (append (seq-subseq l 0 (- (seq-length l) 2))
                            (seq-elt l (1- (seq-length l))))
                  l)))
        (if (not (cl-loop for s in l- if (eq s (intern ".")) collect s))
            (prog1 l-
            (unintern "."))
          (unintern ".")
          (signal 'reader-error "invalid-read-syntax: \".\""))))))

(el-reader/set-macro-character ?\( #'el-reader/read-lisp-list)
(el-reader/set-macro-character ?\" #'el-reader/read-string)
(el-reader/set-macro-character ?? #'el-reader/read-char)

(el-reader/set-macro-character ?\) (lambda (&rest args)
                                     (signal 'unbalanced-sexp nil)))

(el-reader/set-macro-character ?\] (el-reader/get-macro-character ?\)))
(el-reader/set-macro-character
 ?\[ (lambda (stream char)
       (apply #'vector (el-reader/read-delimited-list ?\] stream t))))

(el-reader/set-macro-character
 ?\' (lambda (stream char) `(quote ,(el-reader/read stream t nil t))))
(el-reader/set-macro-character
 ?, (lambda (stream char)
      (let ((next (el-reader/peek-char stream)))
        (if (/= ?@ next)
            `(,(intern ",") ,(el-reader/read stream t nil t))
          (el-reader/getch stream)
          `(,(intern ",@") ,(el-reader/read stream t nil t))))))

(el-reader/set-macro-character
 ?` (lambda (stream char) `(,(intern "`") ,(el-reader/read stream t nil t))))

(defun el-reader/read-comment (stream char)
  (cl-do ((c (el-reader/peek-char stream)
             (progn
               (el-reader/getch stream)
               (el-reader/peek-char stream))))
      ((= c ?\n) (progn
                   (el-reader/getch stream)
                   (el-reader/read stream t nil t)))))

(el-reader/set-macro-character ?\; #'el-reader/read-comment)

(cl-defun el-reader/make-dispatching-function (&optional (readtable *readtable*))
  (let ((macro-funs (make-hash-table)))
    ;; Note that the arguments to set must be a key, a function, and optionally
    ;; a boolean, to indicate whether key shall be non-terminating (non-nil) or
    ;; terminating (nil).
    (lambda (stream char &rest closure-args)
      (-let [(action var val opt-val) closure-args]
        (cond ((eq action :get)
               (gethash var macro-funs))
              ((eq action :set)
               (setf (gethash var macro-funs) (list val opt-val)))
              ((and (not (and action var val))
                    (or action var val))
               (error "Invalid args"))
              (t
               (let ((backup (make-hash-table)))
                 (cl-loop for k being the hash-keys in macro-funs using (hash-value f)
                          do (-let [(f term) f]
                               (setf (gethash k backup)
                                     (el-reader/get-macro-character k readtable))
                               (el-reader/set-macro-character
                                k f term readtable)))
                 (unwind-protect
                     (el-reader/read stream t nil t)
                   ;; On unwind, first clear all set macro chars,
                   (cl-loop for k being the hash-keys in macro-funs using
                            (hash-value f) do
                            (-let [(f term) f]
                              (el-reader/set-macro-character
                               k nil term readtable)))
                   ;; then restore the old ones.
                   (cl-loop for k being the hash-keys in backup using
                            (hash-value f) do
                            (-let [(f term) f]
                              (el-reader/set-macro-character
                               k f term readtable)))))))))))

(cl-defun el-reader/make-dispatching-function (&optional (readtable *readtable*))
  (let ((macro-funs (make-hash-table)))
    ;; Note that the arguments to set must be a key, a function, and optionally
    ;; a boolean, to indicate whether key shall be non-terminating (non-nil) or
    ;; terminating (nil).
    (lambda (stream char &rest closure-args)
      (-let [(action var val opt-val) closure-args]
        (cond ((eq action :get)
               (gethash var macro-funs))
              ((eq action :set)
               (setf (gethash var macro-funs) (list val opt-val)))
              ((and (not (and action var val))
                    (or action var val))
               (error "Invalid args"))
              (t
               (let ((c (el-reader/getch stream)))
                 (if-let ((f (gethash c macro-funs)))
                     (funcall (car f) stream c)
                   (error "Invalid read syntax: \"%c\"" char)))))))))

;; (defun el-reader/get-dispatch-macro-character (disp-fun sub-char &optional
;;                                                          readtable))

(defun el-reader/set-dispatch-macro-function (disp-fun sub-char new-function
                                                        &optional
                                                        non-terminating-p)
  (funcall disp-fun nil nil :set sub-char new-function non-terminating-p))

;;; The main dispatching macro char: #
(el-reader/set-macro-character ?# (el-reader/make-dispatching-function) t)

;;; Function quote: #'func => (function func)
(el-reader/set-dispatch-macro-function
 (car (el-reader/get-macro-character ?#))
 ?' (lambda (stream char)
      `(function ,(el-reader/read stream))))

;;; Read circular objects: #1=(a #1#)
(defun el-reader/read-circle (stream char)
  ;; Unread the first char, as it is part of the number
  (el-reader/getch stream char)
  ;; Set = as a terminating macro char.  It does not need any meaning, as it
  ;; is only used, to end the number.  123= shall read as the number 123, not as
  ;; the symbol 123=.

  (let ((backup-for-= (el-reader/get-macro-character ?=))
        (read-objects (make-hash-table)))
    (el-reader/set-macro-character
     ?=
     (lambda (stream char)
       (error "Invalid read syntax: \"%c\"" char))
     t)
    (let ((n (el-reader/read stream t nil t)))
      (el-reader/set-macro-character ?= (car backup-for-=) (cadr backup-for-=))
      (let ((obj (el-reader/read stream t nil t)))
        (setf (gethash n read-objects) obj)))))

;;;###autoload
(cl-defun el-reader/read (&optional input-stream (eof-error-p t) eof-value
                                    recursive-p)
  (let* ((input-stream (el-reader/get-getch-state input-stream))
         (x (condition-case c
                (el-reader/getch input-stream)
              (end-of-file
               (if eof-error-p
                   (signal (car c) (cdr c))
                 (cl-return-from el-reader/read eof-value))))))
    (cl-labels
        ((has-case-p (c) (not (= (upcase c) (downcase c))))
         (force-alphabetic (c) (let ((z (char-to-string c)))
                                 (put-text-property
                                  0 1 'syntax-type 'constituent z)
                                 (put-text-property
                                  0 1 'traits '(alphabetic) z)
                                 (put-text-property
                                  0 1 'escapedp t z)
                                 z))
         (put-escaped-prop (s &optional (begin 0) (end (length s)))
                           (put-text-property
                            begin end 'escapedp t s)
                           s)
         (switch-case (c) (if (= (upcase c) c)
                              (downcase c)
                            (upcase c)))
         (step-8 (input-stream token)
                 (let ((y (condition-case c
                              (el-reader/getch input-stream)
                            (end-of-file (cl-return-from el-reader/read
                                           (el-reader/process-token token))))))
                   (cond
                    ((el-reader/rt/invalidp *readtable* x)
                     (signal 'reader-error (list "Invalid character" x)))
                    ((el-reader/rt/single-escape-char-p *readtable* y)
                     (let ((z (el-reader/getch input-stream)))
                       (step-8 input-stream
                               (s-concat token (force-alphabetic z)))))
                    ((el-reader/rt/multiple-escape-char-p *readtable* y)
                     (step-9 input-stream (el-reader/defaults-to-str-props x)))
                    ((el-reader/rt/terminating-macro-char-p *readtable* y)
                     (el-reader/getch input-stream y)
                     (el-reader/process-token token))
                    ((el-reader/rt/whitespacep *readtable* y)
                     (when *el-reader/preserve-whitespace*
                       (el-reader/getch input-stream y))
                     (el-reader/process-token token))
                    ((or (el-reader/rt/constituentp *readtable* y)
                         (el-reader/rt/non-terminating-macro-char-p
                          *readtable* y))
                     (step-8 input-stream
                             (s-concat token
                                       (el-reader/defaults-to-str-props y)))))))
         (step-9 (input-stream token)
                 (let ((y (el-reader/getch input-stream)))
                   (cond
                    ((funcall (-orfn
                               (seq-map
                                (lambda (fn)
                                  (-partial fn *readtable*))
                                (list
                                 #'el-reader/rt/constituentp
                                 #'el-reader/rt/terminating-macro-char-p
                                 #'el-reader/rt/non-terminating-macro-char-p
                                 #'el-reader/rt/whitespace-char-p)))
                              y)
                     (step-9
                      input-stream (put-escaped-prop (force-alphabetic y))))
                    ((el-reader/rt/single-escape-char-p *readtable* y)
                     (step-9
                      input-stream (put-escaped-prop
                                    (force-alphabetic
                                     (el-reader/getch input-stream)))))
                    ((el-reader/rt/multiple-escape-char-p *readtable* y)
                     (step-8 token))
                    ((el-reader/rt/invalidp *readtable* y)
                     (signal 'reader-error "Invalid char"))))))
      (cond ((el-reader/rt/invalid-syntax-type-p *readtable* x)
             (signal 'reader-error (list "Invalid char" x)))
            ((el-reader/rt/whitespacep *readtable* x)
             (el-reader/read input-stream eof-error-p eof-value recursive-p))
            ((el-reader/rt/terminating-macro-char-p *readtable* x)
             (let ((*el-reader//allow-single-dot-symbol* nil))
               (funcall (gethash x (el-reader/rt/term-mac-fns *readtable*))
                        input-stream x)))
            ((el-reader/rt/non-terminating-macro-char-p *readtable* x)
             (let ((*el-reader//allow-single-dot-symbol* nil))
               (funcall (gethash x (el-reader/rt/non-term-mac-fns *readtable*))
                        input-stream x)))
            ((el-reader/rt/single-escape-char-p *readtable* x)
             ;; (signal 'reader-error "Single escape is not yet supported")
             (step-8
              input-stream
              (put-escaped-prop
               (force-alphabetic (el-reader/getch input-stream)))))
            ((el-reader/rt/multiple-escape-char-p *readtable* x)
             ;; (signal 'reader-error "Multiple escape is not yet supported")
             ;; Begin an empty token and proceeed with step 9.
             (step-9 input-stream "")
             ;; (let ((y (el-reader/getch input-stream)))
             ;;   ;; Begin an empty token and proceeed with step 9.
             ;;   (step-9 input-stream ""))
             )
            ((el-reader/rt/constituentp *readtable* x)
             (step-8 input-stream (el-reader/defaults-to-str-props x)))
            (t (error "PANIC!!! THIS SHOULD NEVER HAVE HAPPENED!!!")
               ;; So far it has not happened.
               )))))

(provide 'el-reader)

;;; el-reader.el ends here
