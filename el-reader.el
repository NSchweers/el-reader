;;; el-reader.el --- An Advanced reader for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2015 Nathanael Schweers

;; Author: Nathanael Schweers <NSchweers+el-reader@mailbox.org>
;; Created: 09 Nov 2015
;; Keywords: reader
;; Homepage: https://github.com/NSchweers/el-reader
;; Package-Version: 0.0.1
;; Package-Requires: ((seq "1.7") (dash "20151021.113") (dash-functional "20150828.413") (emacs "25.0"))

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

;;; Commentary:

;; A flexible reader for elisp.

;;; Code:

;; TODO: Add some more commentary.
;; The reader algorithm is described here:
;; http://www.lispworks.com/documentation/lw70/CLHS/Body/02_b.htm

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

;; (setf lexical-binding t)
;;;Emacs (especially org-mode) is sometimes a little eratic when it comes to
;;;variables defined with -*- syntax (see first line).  This is why `setf' is
;;;used here.

(eval-and-compile (make-variable-buffer-local 'use-el-reader)
                  (make-variable-buffer-local 'el-reader-bytecode))

;; NOTE: put the following line at the start of any buffer which shall use
;; el-reader (without the comment chars, of course).

;; (eval-and-compile (setf use-el-reader t))

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'dash)
(require 'dash-functional)
;; (require 'hash-utils)
(require 'seq)

(eval-when-compile (require 'cl))

(eval-and-compile
  (defun el-reader//ht (&rest args)
    "Create and return a hashtable.

Keys and values are given alternating in args."
    (let ((h (make-hash-table)))
      (cl-loop for (key value) on args by #'cddr
               do (if (and key value) (puthash key value h)
                    (error "Odd number of arguments passed")))
      h))

  ;; (cl-define-compiler-macro el-reader//ht (&rest args)
;;       "This compiler macro performs loop unrolling.

;; Unfortunately it does a maximal unroll."
;;       (cl-labels ((proc-entry (k v h) `(puthash ,k ,v ,h)))
;;         (if (cl-oddp (length args))
;;             (error "Odd number of args passed")
;;           (let ((h (cl-gensym)))
;;             `(let ((,h (make-hash-table)))
;;                ,@(cl-loop for (key value) on args by #'cddr
;;                           collect (proc-entry key value h))
;;                ,h)))))

;;; In COMMON LISP traits are hardwired to the parser, we should not copy this
;;; mistake.  Also, define a mapping from digits to values.  I.e. make it
;;; possible for *el-reader/read-base* = 36 to have Z mean (dec 10) and A (dec 35).
  (defclass el-reader/readtable ()
    ((invalid-chars :initarg :invalid :initform nil :type list)
     ;; Include as many whitespace chars as possible.  Here we have the regular
     ;; space (?\s -- 32), nobreak space (?\  -- 160) and thin space (?\  --
     ;; 8239).
     (whitespace-chars :initarg :whitespace-chars
                       :initform '(?\s ?\t ?\n ?\e ?\f ?\  ?\ )
                       :type list)
     (single-escape-chars :initarg :single-escape-chars :initform nil
                          :type list)
     (multiple-escape-chars :initarg :multiple-escape-chars :initform nil
                            :type list)
     (constituent-chars :initarg :constituent-chars :initform nil :type list)
     (traits :initarg :traits :initform (el-reader//ht) :type hash-table)
     (term-mac-fns :initarg :term-mac-fns :initform (el-reader//ht)
                   :type hash-table)
     (non-term-mac-fns :initarg :non-term-mac-fns :initform (el-reader//ht)
                       :type hash-table)
     (readtable-case :initarg :readtable-case :initform :preserve)
     (char-to-num :initarg :char-to-num :initform
                  (el-reader//ht 48 0 49 1 50 2 51 3 52 4 53 5 54 6 55 7 56 8
                                 57 9 97 10 98 11 99 12 100 13 101 14 102 15
                                 103 16 104 17 105 18 106 19 107 20 108 21 109
                                 22 110 23 111 24 112 25 113 26 114 27 115 28
                                 116 29 117 30 118 31 119 32 120 33 121 34 122
                                 35 65 10 66 11 67 12 68 13 69 14 70 15 71 16
                                 72 17 73 18 74 19 75 20 76 21 77 22 78 23 79
                                 24 80 25 81 26 82 27 83 28 84 29 85 30 86 31
                                 87 32 88 33 89 34 90 35))
     (classic-dispatch-functions :initarg :classic-dispatch-functions
                                 :initform (el-reader//ht))))

  (defmacro el-reader//make-public-accessors (class &optional prefix)
    (cons 'progn
          (seq-map
           (lambda (e)
             `(progn
                (defun ,(intern (s-concat (if prefix (symbol-name prefix) "")
                                          (symbol-name e)))
                    (obj)
                  ,(s-concat "Extract the element " (symbol-name e)
                             " from OBJ. ")
                  (slot-value obj ',e))
                (cl-defmethod (setf ,(intern (s-concat
                                              (if prefix
                                                  (symbol-name prefix)
                                                "")
                                              (symbol-name e))))
                    (new-elt (obj el-reader/readtable))
                  (setf (slot-value obj ',e) new-elt))))
           (seq-map #'eieio-slot-descriptor-name (eieio-class-slots class)))))

  (el-reader//make-public-accessors el-reader/readtable el-reader//rt/)

  (defun el-reader/make-default-elisp-readtable ()
    (make-instance
     'el-reader/readtable
     :whitespace-chars
     '(?\s ?\t ?\n ?\e ?\f)
     ;; Escaping is supported, yet no case conversion is performed.  The main
     ;; reason to perform escaping is to allow spaces and the like in symbols.
     ;; While it is supported, by default we do not register any multiple escape
     ;; chars, just in case someone used the | char in a symbol without
     ;; intending it to be escaped.
     :single-escape-chars '(?\\)
     ;; :multiple-escape-chars '(?|)
     ;; :terminating-macro-chars '(?\" ?' ?\( ?\) ?, ?\; ?` ??)
     ;; :non-terminating-macro-chars '(?#)
     :constituent-chars '(?\b ?! ?$ ?% ?& ?* ?+ ?- ?. ?/ 48 49 50
                              51 52 53 54 55 56 57 ?: ?< ?= ?> ??
                              ?@ 65 66 67 68 69 70 71 72 73 74 75
                              76 77 78 79 80 81 82 83 84 85 86 87
                              88 89 90 ?\[ ?\] ?^ ?_ 97 98 99 100
                              101 102 103 104 105 106 107 108 109
                              110 111 112 113 114 115 116 117 118
                              119 120 121 122 ?{ ?} ?~)
     :traits (el-reader//ht 'invalid '(?\b ?\t ?\n ?\f ?\r ?\s)
                 
                            'alphabetic '(?! ?\" ?# ?$ ?% ?& ? ?\( ?\) ?* ?,
                                             ?\; ?< ?= ?> ?@ ?[ ?\\ ?] ?^ ?_
                                             ?` ?| ?~ ?{ ?} ?+ ?- ?. ?/)
                 
                            'alphadigit '(48 49 50 51 52 53 54 55 56 57 65
                                             66 67 68 69 70 71 72 73 74 75
                                             76 77 78 79 80 81 82 83 84 85
                                             86 87 88 89 90 97 98 99 100 101
                                             102 103 104 105 106 107 108 109
                                             110 111 112 113 114 115 116 117
                                             118 119 120 121 122)
                 
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

  (defvar *el-reader/readtable*
    (el-reader/make-default-elisp-readtable))

  (defvar *el-reader/read-base* 10)

  (defvar *el-reader/preserve-whitespace* nil
    "Tells read that it was called as `read-preserving-whitespace'.

Why the Common Lisp folks felt the need to make this into a
separate function instead of an argument is beyond me.

This variable should not be used directly.  It is set by
`read-preserving-whitespace' before calling `read'.")

  (defvar *el-reader//allow-single-dot-symbol* nil
    "Whether to allow a symbol named `.'.
Do not use this variable!  It is used internally in the reader.
Only a function called by `read' and the list reading code use
it.  It shall stay that way!")

  (defvar *el-reader//read-objects* nil
    "Holds the associations (alist) of previously read objects.

#1=(...) replaces all occurences of #1# with a reference to (...).

This is done by associating 1 with a dummy cons (cons nil nil),
replacing #1# with said cons, and then replacing all dummy conses
with the proper reference. ")

;;   (defvar *el-reader//circular-read-functions* nil
;;     "This is used internally by the reader macros for #n= and #n#.

;; Not part of any public interface.  Assume nothing about it.")

  (defvar *el-reader/repeat-read* (cons nil nil)
    "This is a marker object, which may be returned by a read macro function.

This is done in order to signal that the read process shall be repeated, thus
forming a trampoline.  If Emacs had proper tail-call elimination, this would not
have been necessary."))

(defun el-reader//memhash (key table)
  (and (hash-table-p table)
       (or (gethash key table)
           (not (gethash key table t)))))

(cl-defmacro el-reader//removable-advice
    ((where place &optional remover props) args &body body)
  "Add BODY as advice according to WHERE, PLACE and PROPS.

Returns a closure which removes said advice.  Said closure is
lexically bound to the symbol REMOVER (in body).  The closure is present in the
value slot, so it must be called via funcall or apply."
  (let ((name (cl-gensym "λ:"))
        (rem (cl-gensym "φ:")))
    `(let ((,(or remover rem)
            (lambda ()
              (remove-function ,place #',name)
              (fmakunbound ',name))))
       (defun ,name ,args ,@body)
       (add-function ,where ,place #',name ,props)
       ,(or remover rem))))

(define-error 'end-of-file "End of file reached")

(defclass el-reader//string-reader-state ()
  ((string :initarg :string :type string)
   (pos :initarg :pos :initform 0 :type integer)))

(defclass el-reader//function-read-state ()
  ((fn :initarg :function)))

(cl-defgeneric el-reader/getch (stream &optional char)
  "Read a character using OBJ as source.  If CHAR is given, unread that char.

If at end of stream, throws end-of-file")

(cl-defun el-reader/peek-char (stream)
  "Return the next char in the stream without removing it."
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
          ;; (end-of-buffer (signal 'end-of-file "End of buffer reached"))
          (signal 'end-of-file "End of buffer reached")
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
           ;; (end-of-buffer (signal 'end-of-file "End of buffer reached"))
           (signal 'end-of-file "End of buffer reached")
         (prog1 (char-after)
             (incf (marker-position m))))))))

(cl-defmethod el-reader/getch ((s el-reader//string-reader-state)
                               &optional char)
  (with-slots ((s string) pos) s
    (if char
        (progn
          (when (and char (= pos 0))
            (error "Unreading to before the string begins"))
          (when (and char (not (= char (elt s (1- pos)))))
            (error "Unreading a different char than was read"))
          (decf pos))
      (if (<= (length s) pos)
          (signal 'end-of-file "End of string")
        (prog1 (elt s pos)
          (incf pos))))))

(cl-defmethod el-reader/getch ((fn el-reader//function-read-state)
                               &optional char)
  (funcall (slot-value fn 'fn) char))

(cl-defgeneric el-reader//get-getch-state (obj)
  "Return an object which is suitable for `el-reader/getch'.

The default method checks if OBJ is a function, as cl-defgeneric
cannot dispatch on functions.  Otherwise, OBJ is returned as is."
  (if (functionp obj)
      (if (eq obj #'get-file-char)
          (make-instance
           'el-reader//function-read-state
           :function
           (let ((unread-chars))
             (lambda (&optional c)
               (if c (progn (push c unread-chars) nil)
                 (if unread-chars
                     (pop unread-chars)
                   (funcall obj))))))
        (make-instance 'el-reader//function-read-state :function obj))
    obj))

;; (cl-defmethod el-reader//get-getch-state ((gfc (eql 'get-file-char)))
;;   (message "Appropriate method for get-getch-state called!")
;;   (make-instance
;;    'el-reader//function-read-state
;;    :function
;;    (let ((unread-chars))
;;      (lambda (&optional c)
;;        (if c (progn (push c unread-chars) nil)
;;          (if unread-chars
;;              (pop unread-chars)
;;            (funcall gfc)))))))

(cl-defmethod el-reader//get-getch-state ((s string))
  (make-instance 'el-reader//string-reader-state :string s))

(cl-defmethod el-reader//get-getch-state ((_stdin (eql t)))
  (el-reader//get-getch-state (read-from-minibuffer "Lisp expression: ")))

(cl-defmethod el-reader//get-getch-state ((_stdin (eql nil)))
  (el-reader//get-getch-state standard-input))

;; (defclass el-reader/macro-fn ()
;;   ((char :initarg :char :type character)
;;    (fn :initarg :fn)))

;; get-macro-character char &optional readtable => function, non-terminating-p
(cl-defun el-reader/get-macro-character (char &optional
                                           (readtable *el-reader/readtable*))
  (--if-let (gethash char (el-reader//rt/term-mac-fns readtable))
      (list it nil)
    (list (gethash char (el-reader//rt/non-term-mac-fns readtable)) t)))

;; set-macro-character char new-function &optional non-terminating-p readtable
;; => t
(cl-defun el-reader/set-macro-character (char new-function &optional
                                           non-terminating-p
                                           (readtable *el-reader/readtable*))
  (remhash char (el-reader//rt/non-term-mac-fns readtable))
  (remhash char (el-reader//rt/term-mac-fns readtable))
  (when new-function
    (if non-terminating-p
        (setf (gethash char (el-reader//rt/non-term-mac-fns readtable))
              new-function)
      (setf (gethash char (el-reader//rt/term-mac-fns readtable))
            new-function))
    t))

(defclass el-reader//result ()
  ((success :initarg :success :initform nil :type symbol)
   (token :initarg :token :type string)
   (pos :initarg :pos :type (or integer marker))
   (newpos :initarg :newpos :type (or integer marker null)) ;or marker?
   (result-string :initarg :result-string :type (or string null))
   (rest :initarg :rest :type (or string null))
   (result :initarg :result)
   (metadata :initarg :metadata :initform nil))
  "A type for the result of a parse.")

(defun el-reader//result/success-p (r)
  (and (el-reader//result-p r) (slot-value r 'success)))

(defun el-reader//make-result (success token pos newpos
                                      result-string rest result)
  (make-instance 'el-reader//result :success success :token token :pos
                 pos :newpos newpos :result-string result-string :rest rest
                 :result result))

(defun el-reader//make-failed (token pos)
  (make-instance 'el-reader//result :token token :pos pos :newpos nil
                 :result-string nil :rest nil :result nil))

(defun el-reader//rt/constituentp (rt char)
  "Returns non-nil if CHAR is a constituent char.

This is the case if it is either a member of the slot
CONSTITUENT-CHARS, or not a member of the other classes"
  (or (not (null (member char (el-reader//rt/constituent-chars rt))))
      (not
       (funcall
        (apply #'-orfn
               (seq-map
                (lambda (f) (-partial f rt))
                (list  #'el-reader//rt/invalidp
                      #'el-reader//rt/terminating-macro-char-p
                      #'el-reader//rt/non-terminating-macro-char-p
                      #'el-reader//rt/whitespacep
                      #'el-reader//rt/single-escape-char-p
                      #'el-reader//rt/multiple-escape-char-p)))
        char))))

(defun el-reader//rt/invalid-syntax-type-p (rt char)
  "Return non-nil if CHAR is of syntax type invalid."
  (when (member char (el-reader//rt/invalid-chars rt))
    t))

(defun el-reader//rt/invalid-trait-p (rt char)
  "Return non-nil if CHAR has the invalid trait.

Note that this may wrongly return non-nil, if a trait was set for
a non-constituent character."
  (when (member char (gethash 'invalid (el-reader//rt/traits rt)))
    t))

(defun el-reader//rt/invalidp (rt char)
  "Return non-nil if CHAR is either both constituent and invalid, or of
syntax-type invalid."
  (or (el-reader//rt/invalid-syntax-type-p rt char)
      (el-reader//rt/invalid-trait-p rt char)))

(defun el-reader//rt/whitespacep (rt char)
  (not (null (member char (el-reader//rt/whitespace-chars rt)))))

(defun el-reader//rt/terminating-macro-char-p (rt char)
  (not (null (member char (cl-loop for k being the hash-keys in
                                   (slot-value rt 'term-mac-fns)
                                   collect k)))))

(defun el-reader//rt/non-terminating-macro-char-p (rt char)
  (not (null (member char (cl-loop for k being the hash-keys in
                                   (slot-value rt 'non-term-mac-fns)
                                   collect k)))))

(defun el-reader//rt/single-escape-char-p (rt char)
  (not (null (member char (el-reader//rt/single-escape-chars rt)))))

(defun el-reader//rt/multiple-escape-char-p (rt char)
  (not (null (member char (el-reader//rt/multiple-escape-chars rt)))))

;;; Define read macros here.

(defun el-reader/read-string (stream char)
  (el-reader/getch stream char)
  (advice-remove 'read #'read@el-reader//replace-read)
  (prog1 (cl-values (read (-partial #'el-reader/getch stream)))
    (advice-add 'read :around #'read@el-reader//replace-read)))

;; (defun el-reader/read-string (stream char)
;;   (let ((str nil))
;;     (do ((c (el-reader/getch stream) (el-reader/getch stream)))
;;         ((and (= c ?\") (not (= (car str) ?\\)))
;;          (push c str)))))

(defun el-reader/read-char (stream _char)
  (let* ((ch1 (el-reader/getch stream))
         (str (s-concat "?" (char-to-string ch1))))
    (when (= ch1 ?\\)
      (setf str (s-concat str (char-to-string (el-reader/getch stream)))))
    (prog2
        (advice-remove 'read #'read@el-reader//replace-read)
        (cl-values (read str))
      (advice-add 'read :around #'read@el-reader//replace-read))))

;; (defun el-reader//defaults-to-str-props (char)
;;   (let ((s (char-to-string char)))
;;     (cl-labels
;;         ((adjust-macro-names
;;           (type)
;;           (if (string-suffix-p "macro" (symbol-name type))
;;               (intern (s-concat (symbol-name type) "-char"))
;;             type))
;;          (put-syntax-type
;;           (type)
;;           (when (cl-member char
;;                            (funcall
;;                             (intern (s-concat "el-reader/rt/"
;;                                               (symbol-name type) "-chars"))
;;                             *el-reader/readtable*))
;;             (if (get-text-property 0 type s)
;;                 (error "More than one syntax type")
;;               (put-text-property 0 1 'syntax-type
;;                                  (adjust-macro-names type) s)))))
;;       (seq-do
;;        (lambda (sym) (put-syntax-type sym))
;;        '(terminating-macro
;;          non-terminating-macro
;;          constituent
;;          whitespace
;;          single-escape
;;          multiple-escape))
;;       (put-text-property
;;        0 1 'traits 
;;        (cl-loop for k being the hash-keys in (el-reader//rt/traits *el-reader/readtable*)
;;                 if (cl-member char (gethash k (el-reader//rt/traits
;;                                                *el-reader/readtable*)))
;;                 collect k)
;;        s)
;;       s)))

(defun el-reader//defaults-to-str-props (char)
  (let ((s (char-to-string char)))
    (cl-labels
        ((put-syntax-type
          (type)
          (when (funcall
                 (intern
                  (s-concat
                   "el-reader//rt/"
                   (symbol-name type)
                   (if (string-match-p "-" (symbol-name type))
                       "-p"
                     "p")))
                 *el-reader/readtable* char)
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
       (cl-loop for k being the hash-keys in (el-reader//rt/traits *el-reader/readtable*)
                if (cl-member char (gethash k (el-reader//rt/traits
                                               *el-reader/readtable*)))
                collect k)
       s)
      s)))

(cl-defun el-reader//put-token-props (token prop val &optional
                                           (start (1- (length token)))
                                           (end start))
  (put-text-property start end prop val token)
  token)

(defun el-reader//token-constituent-p (token pos)
  (get-text-property pos 'syntax-type token))

(defun el-reader//token-alphabetic-p (token pos)
  (let ((p (get-text-property pos 'traits token)))
    (and (listp p) (eq (car p) 'alphabetic))))

;;; This is a recursive descent parser for number types.  This may be stripped
;;; away in a future version.  The main reason to strip it, is that Emacs’
;;; default reader may be used.  The main reason to keep it, is that this
;;; version may be easier to extend.  For now it stays.
;;; NOTE:  This /must/ stay, at least for the moment.  The reason is that
;;; otherwise reading of #b, #o, #x and #NNr... via macrochars cannot be
;;; implemented.  The reason for this is that *el-reader/read-base* is needed.


;;; Create classes for each parse result (digit, exponent, etc.)

(defclass el-reader//syntax-element ()
  ((value :initarg :value)))

(defclass el-reader//digit (el-reader//syntax-element)
  ((base :initarg :base :initform 10 :type integer)))
(defclass el-reader//decimal-digit (el-reader//digit)
  ((base :initform 10)))

(defclass el-reader//exponent-marker (el-reader//syntax-element) ())
(defclass el-reader//exponent (el-reader//syntax-element) ())
(defclass el-reader//char (el-reader//syntax-element) ())

(defclass el-reader//sign (el-reader//syntax-element) ())
(defclass el-reader//plus-sign (el-reader//sign) ())

(defclass el-reader//decimal-point (el-reader//syntax-element) ())

(defclass el-reader//inf-marker (el-reader//syntax-element) ())
(defclass el-reader//nan-marker (el-reader//syntax-element) ())

(defclass el-reader//float (el-reader//syntax-element) ())

;; We have a number of functions which combine two parsing functions to create a
;; new one.  Each of them takes two functions which take a token and starting
;; position each.  The resulting function has the same signature.  This way,
;; functions can be chained together.  In types:
;; (token -> pos -> plist) -> (token -> pos -> plist) -> (token -> pos -> plist)

(defun el-reader//ensure-complete-token (fn)
  (lambda (token pos)
    (let ((res (funcall fn token pos)))
      (cond ((and (slot-value res 'success)
                  (or (not (string-empty-p (slot-value res 'rest)))
                      (< (slot-value res 'newpos) (length token))))
             (el-reader//make-failed token pos))
            (t res)))))

;; (defun el-reader//parse-seq (&rest fns)
;;   (when (null fns)
;;     (error "At least one argument must be given"))
;;   (lambda (token pos)
;;     (let ((r (seq-reduce
;;               (lambda (a f)
;;                 (if (slot-value a 'success)
;;                     (with-slots ((a-token token)
;;                                  (a-newpos newpos)
;;                                  (a-pos pos)
;;                                  (a-result result))
;;                         a
;;                       (let ((tmp (funcall f a-token
;;                                           a-newpos)))
;;                         (with-slots ((tmp-success success)
;;                                      (tmp-newpos newpos)
;;                                      (tmp-rest rest)
;;                                      (tmp-result result))
;;                             tmp
;;                           (if tmp-success
;;                               (el-reader//make-result
;;                                t token pos tmp-newpos
;;                                (substring a-token
;;                                           a-pos
;;                                           tmp-newpos)
;;                                tmp-rest
;;                                (cons tmp-result a-result))
;;                             (el-reader//make-failed token pos)))))
;;                   (el-reader//make-failed token pos)))
;;               (cdr fns)
;;               (let ((tmp (funcall (car fns) token pos)))
;;                 (clone tmp :result (list (slot-value tmp 'result)))))))
;;       (clone r :result (reverse (slot-value r 'result))))))

(defun el-reader//parse-seq (&rest fns)
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
                              (el-reader//make-result
                               t token pos tmp-newpos
                               (substring a-token
                                          a-pos
                                          tmp-newpos)
                               tmp-rest
                               (cons tmp-result a-result))
                            (el-reader//make-failed token pos)))))
                  (el-reader//make-failed token pos)))
              (cdr fns)
              (let ((tmp (funcall (car fns) token pos)))
                (setf (slot-value tmp 'result) (list (slot-value tmp 'result)))
                tmp))))
      (setf (slot-value r 'result) (reverse (slot-value r 'result)))
      r)))

;; (defun el-reader//parse-alt (&rest fns)
;;   (when (null fns)
;;     (error "At least one argument must be given"))
;;   (lambda (token pos)
;;     (cl-labels ((helper (fns)
;;                         (if (null fns)
;;                             (el-reader//make-failed token pos)
;;                           (let ((res (funcall (car fns) token pos)))
;;                             (if (slot-value res 'success)
;;                                 res
;;                               (helper (cdr fns)))))))
;;       (helper fns))))

(defun el-reader//parse-alt (&rest fns)
  (when (null fns)
    (error "At least one argument must be given"))
  (lambda (token pos)
    (let ((n 0)
          (fs fns)
          res)
      (while fs
        (setf res (funcall (car fs) token pos))
        (if (slot-value res 'success)
          ;; This will abort the loop, hence return res.
            (setf fs nil)
          (setf fs (cdr fs))))
      (push (list :type :alt :alt (if (>= n (length fns)) nil n))
            (slot-value res 'metadata))
      res)))

(defun el-reader//parse-optional (fn)
  (lambda (token pos)
    (let ((r (funcall fn token pos)))
      (if (slot-value r 'success)
          (progn
            (push (list :type :optional :present t) (slot-value r 'metadata))
            r)
        (let ((r- (el-reader//make-result t token pos pos "" (substring token pos)
                                          nil)))
          (push (list :type :optional :present nil) (slot-value r- 'metadata))
          r-)))))

;; (defun el-reader//parse-kleene-star (fn)
;;   (lambda (token pos)
;;     (cl-labels
;;         ((helper
;;           (res)
;;           (let ((tmp (funcall fn token (slot-value res 'newpos))))
;;             (if (not (slot-value tmp 'success))
;;                 (clone res :result (reverse (slot-value res 'result)))
;;               (helper
;;                (el-reader//make-result t token (slot-value res 'pos)
;;                                    (slot-value tmp 'newpos)
;;                                    (substring token (slot-value res 'pos)
;;                                               (slot-value tmp 'newpos))
;;                                    (substring token (slot-value tmp 'newpos))
;;                                    (cons (slot-value tmp 'result)
;;                                          (slot-value res 'result))))))))
;;       (let ((res (funcall fn token pos)))
;;         (if (not (slot-value res 'success))
;;             (el-reader//make-result t token pos pos "" (substring token pos)
;;                                     nil)
;;           (helper (clone res :result (list (slot-value res 'result)))))))))

(defun el-reader//parse-kleene-star (fn)
  (lambda (token pos)
    (do* ((res
           (el-reader//make-result t token pos pos ""
                                   (substring token pos) nil)
           (el-reader//make-result t token (slot-value res 'pos)
                                   (slot-value tmp 'newpos)
                                   (substring token (slot-value res 'pos)
                                              (slot-value tmp 'newpos))
                                   (substring token
                                              (slot-value tmp 'newpos))
                                   (cons (slot-value tmp 'result)
                                         (slot-value res 'result))))
          (tmp (funcall fn token (slot-value res 'newpos))
               (funcall fn token (slot-value tmp 'newpos))))
        ((not (slot-value tmp 'success))
         (progn
           (setf (slot-value res 'result)
                 (nreverse (slot-value res 'result)))
           res)))))

(defun el-reader//parse-plus (fn)
  (-compose
   (lambda (r)
     (if (< (length (slot-value r 'result)) 1)
         (with-slots (token pos) r
           (el-reader//make-failed token pos))
       r))
   (el-reader//parse-kleene-star fn)))

(defun el-reader//parse-single-char (c)
  (lambda (token pos)
    (if (= c (string-to-char (substring token pos)))
        (el-reader//make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token (1+ pos))
                                (make-instance 'el-reader//char :value c))
      (el-reader//make-failed token pos))))

(defun el-reader//parse-single-char-nocase (c)
  (lambda (token pos)
    (if (= (downcase c) (downcase (string-to-char (substring token pos))))
        (el-reader//make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token (1+ pos))
                                (make-instance 'el-reader//char
                                               :value (downcase c)))
      (el-reader//make-failed token pos))))

(defun el-reader//parse-exponent-marker (token pos)
  (if (eq (get-text-property pos 'syntax-type token) 'constituent)
      (-if-let (r (seq-filter
                   (-compose (-partial (lambda (s1 s2)
                                         (if (string-suffix-p s1 s2) s2))
                                       "exponent-marker")
                             #'symbol-name)
                   (get-text-property pos 'traits token)))
          (if (not (= (length r) 1))
              (error "Ambiguous exponent sign")
            (el-reader//make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token pos)
                                (make-instance 'el-reader//exponent-marker
                                               :value (car r))))
        (el-reader//make-failed token pos))
    (el-reader//make-failed token pos)))

(defun el-reader//parse-plus-sign (token pos)
  (if (and (eq (get-text-property pos 'syntax-type token) 'constituent)
           (cl-member 'plus-sign (get-text-property pos 'traits token)))
      (el-reader//make-result
       t token pos (1+ pos)
       (substring token pos (1+ pos))
       (substring token (1+ pos))
       (make-instance 'el-reader//sign :value 'plus-sign))
    (el-reader//make-failed token pos)))

(defun el-reader//parse-sign (token pos)
  (if (eq (get-text-property pos 'syntax-type token) 'constituent)
      (-if-let (r (seq-filter
                   (-compose (-partial
                              (lambda (s1 s2) (if (string-suffix-p s1 s2) s2))
                              "sign")
                             #'symbol-name)
                   (get-text-property pos 'traits token)))
          (if (not (= (length r) 1))
              (error "Ambiguous sign character")
            (el-reader//make-result t token pos (1+ pos)
                                (substring token pos (1+ pos))
                                (substring token (1+ pos))
                                (make-instance 'el-reader//sign
                                               :value (car r))))
        (el-reader//make-failed token pos))
    (el-reader//make-failed token pos)))

(defun el-reader//parse-decimal-point (token pos)
  (if (or (cl-member 'dot (get-text-property pos 'traits token))
          (cl-member 'decimal-point (get-text-property pos 'traits token)))
      (el-reader//make-result
       t token pos (1+ pos)
       (substring token pos (1+ pos))
       (substring token (1+ pos))
       (make-instance 'el-reader//decimal-point
                      :value (substring token pos (1+ pos))))
    (el-reader//make-failed token pos)))

(defun el-reader//parse-digit (token pos)
  (if (and (eq (get-text-property pos 'syntax-type token) 'constituent)
           (cl-member 'alphadigit (get-text-property pos 'traits token))
           (cl-member (string-to-char (substring token pos (1+ pos)))
                      (hash-table-keys
                       (el-reader//rt/char-to-num *el-reader/readtable*)))
           (< (gethash (string-to-char (substring token pos (1+ pos)))
                       (el-reader//rt/char-to-num *el-reader/readtable*))
              *el-reader/read-base*))
      (el-reader//make-result t token pos (1+ pos)
                              (substring token pos (1+ pos))
                              (substring token (1+ pos))
                              (make-instance
                               'el-reader//digit
                               :value (gethash
                                       (string-to-char
                                        (substring token pos (1+ pos)))
                                       (el-reader//rt/char-to-num
                                        *el-reader/readtable*))
                               :base *el-reader/read-base*))
    (el-reader//make-failed token pos)))

(defun el-reader//parse-decimal-digit (token pos)
  (let ((res (el-reader//parse-digit token pos)))
    (cond ((not (slot-value res 'success))  (el-reader//make-failed token pos))
          ((not (= (slot-value (slot-value res 'result) 'base) 10))
           (el-reader//make-failed token pos))
          (t (progn
               (setf (slot-value res 'result)
                     (make-instance 'el-reader//decimal-digit
                                    :value (slot-value (slot-value res 'result) 'value)
                                    :base 10))
               res)))))

;; (defvar *el-reader//parse-inf-marker*
;;   (el-reader//parse-seq
;;    (el-reader//parse-single-char ?+)
;;    (el-reader//parse-single-char ?I)
;;    (el-reader//parse-single-char ?N)
;;    (el-reader//parse-single-char ?F)))

;; (defun el-reader//parse-inf-marker (token pos)
;;   (funcall *el-reader//parse-inf-marker* token pos))

;; (defvar *el-reader//parse-nan-marker*
;;   (el-reader//parse-seq
;;    (el-reader//parse-single-char ?+)
;;    (el-reader//parse-single-char ?N)
;;    (el-reader//parse-single-char ?a)
;;    (el-reader//parse-single-char ?N)))

;; (defun el-reader//parse-nan-marker (token pos)
;;   (funcall *el-reader//parse-nan-marker* token pos))

;; (defvar *el-reader//parse-exponent*
;;       (el-reader//parse-seq #'el-reader//parse-exponent-marker
;;                             (el-reader//parse-optional #'el-reader//parse-sign)
;;                             (el-reader//parse-plus #'el-reader//parse-digit)))

;; (defun el-reader//parse-exponent (token pos)
;;   (funcall
;;    (el-reader//parse-seq #'el-reader//parse-exponent-marker
;;                      (el-reader//parse-optional #'el-reader//parse-sign)
;;                      (el-reader//parse-plus #'el-reader//parse-digit))
;;    token pos))

(defun el-reader//same-base-p (digits)
  (let ((first-base (slot-value (car digits) 'base)))
    (seq-every-p (lambda (it) (= first-base (slot-value it 'base)))
                 (cdr digits))))

(defun el-reader//proper-float-p (float)
  (let ((res (slot-value float 'result)))
    (and (seq-every-p #'el-reader//digit-p (second res))
         (= (slot-value (second res) 'base) 10)
         (el-reader//same-base-p (append (second res)
                                        (fourth res))))))

(defun el-reader//adjust-for-sign (num sign)
  (let ((sign-fn (if (and (el-reader//sign-p sign)
                          (eq (slot-value sign 'value)
                              'minus-sign))
                     #'- #'identity)))
    (funcall sign-fn num)))

(defun el-reader//digit-list->int (digits)
  (if (and (not (null digits)) (listp digits)
           (not (and (= (length digits) 1)
                     (null (car digits))))
           (el-reader//same-base-p digits))
      (let ((base (slot-value (car digits) 'base))
            (digits (reverse (seq-map (lambda (x) (slot-value x 'value))
                                      digits))))
        (car (seq-reduce
              (lambda (acc it)
                (let ((val (car acc))
                      (place (cadr acc)))
                  (list (+ val (* it (expt base place)))
                        (1+ place))))
              (cdr digits) (list (car digits) 1))))
    0))

(defun el-reader//make-int (sign-and-digits)
  (el-reader//adjust-for-sign
   (el-reader//digit-list->int
    (cadr sign-and-digits))
   (car sign-and-digits)))

;; (defun el-reader//drop-trailing-zeros (digits)
;;   (reverse (el-reader//drop-leading-zeros (reverse digits))))

(defun el-reader//drop-leading-zeros (digits)
  "DIGITS is a seq of digit objects (`el-reader//digit').
Leading zeros are dropped, the rest is returned as is."
  (or (seq-drop-while (lambda (d) (zerop (slot-value d 'value)))
                      digits)
      ;; If `seq-drop-while' returns nil, we only had zeros.  We still need one
      ;; of them, though (nil will crash the rest, so we keep one of them)
      (list (car digits))))

(defun el-reader//drop-trailing-zeros (digits)
  "See `el-reader//drop-leading-zeros'."
  (seq-reverse (el-reader//drop-leading-zeros (seq-reverse digits))))

(defun el-reader//pf/get-sign-fn (num)
  (let ((sign (car num)))
    (if (and (el-reader//sign-p sign)
             (eq (slot-value sign 'value) 'minus-sign))
        #'-
      #'identity)))

(defun el-reader//pf/get-int-part (dec-digits)
  ;; (el-reader//digit-list->int
  ;;  (el-reader//drop-leading-zeros dec-digits))
  (el-reader//digit-list->int dec-digits))

(defun el-reader//pf/get-post-dec-places (mantissa)
  (let* ((digits (el-reader//drop-trailing-zeros mantissa))
         (digit-count (length digits))
         (int (or (el-reader//pf/get-int-part digits) 0.0)))
    (* int (expt 10 (- digit-count)))))

(defun el-reader//pf/fe (float)
  (let* ((left (not (or (null (third float))
                        (and (listp (third float))
                             (= (length (third float)) 2)))))
         (dec-digits (cadr float))
         ;; according to the byte-compiler, dec-point? is never used.
         ;; (dec-point? (if left
         ;;                 (third float)
         ;;               (car (third float))))
         (mantissa (if left (fourth float)
                     (second (third float))))
         (exponent (if left (fifth float) (fourth float))))
    (* (funcall
        (el-reader//pf/get-sign-fn float)
        (+ (el-reader//pf/get-int-part dec-digits)
           (el-reader//pf/get-post-dec-places mantissa)))
       (if exponent
           (expt 10 (funcall (el-reader//pf/get-sign-fn
                              (list (second exponent)))
                             (el-reader//pf/get-int-part (third exponent))))
         1.0))))

;; The grammar for floating point numbers has been altered, as CL does not
;; support syntax for NaN and infinity, while Emacs-Lisp does. 

;; In the original grammar
;; (http://www.lispworks.com/documentation/lw70/CLHS/Body/02_ca.htm) `exponent'
;; is replaced with `exponent-or-special-number-marker'

;; exponent ::= exponent-marker [sign] {digit}+

;; infinity-marker ::= exponent-marker '+' 'INF'

;; NaN-marker ::= exponent-marker '+' 'NaN'

;; exponent-or-special-number-marker ::= exponent | infinity-marker


;; Note: This is the original grammar:

;; numeric-token  ::=  integer |
;; 				   ratio   |
;; 				   float       
;; integer        ::=  [sign]
;; 				   decimal-digit+
;; 				   decimal-point |
;; 				   [sign]
;; 				   digit+      
;; ratio          ::=  [sign]
;; 				   {digit}+
;; 				   slash
;; 				   {digit}+    
;; float          ::=  [sign]
;; 				   {decimal-digit}*
;; 				   decimal-point
;; 				   {decimal-digit}+
;; 				   [exponent]  
;;                     | 
;; 				   [sign]
;; 				   {decimal-digit}+
;; 				   [decimal-point
;; 					   {decimal-digit}*]
;; 				   exponent    
;; exponent       ::=  exponent-marker
;; 				   [sign]
;; 				   {digit}+    
                                       
;; sign---a sign.                         
;; slash---a slash                        
;; decimal-point---a dot.                        
;; exponent-marker---an exponent marker.                        
;; decimal-digit---a digit in radix 10.                        
;; digit---a digit in the current input radix

;; Note: This is the modified grammar to accomodate ELisps float syntax:
;; numeric-token  ::=  integer |
;; 				   ratio   |
;; 				   float       
;; integer        ::=  [sign]
;; 				   decimal-digit+
;; 				   decimal-point |
;; 				   [sign]
;; 				   digit+      
;; ratio          ::=  [sign]
;; 				   {digit}+
;; 				   slash
;; 				   {digit}+    
;; float          ::=  [sign]
;; 				   {decimal-digit}*
;; 				   decimal-point
;; 				   {decimal-digit}+
;; 				   [extension]  
;;                     | 
;; 				   [sign]
;; 				   {decimal-digit}+
;; 				   [decimal-point
;; 					   {decimal-digit}*]
;; 				   extension
;; exponent       ::=  exponent-marker
;; 				   [sign]
;; 				   {digit}+
;; inf-marker     ::= exponent-marker '+INF'
;; nan-marker     ::= exponent-marker '+NaN'
;; extension      ::= exponent | exponent-marker (inf-marker | nan-marker)
                                       
;; sign---a sign.                         
;; slash---a slash                        
;; decimal-point---a dot.                        
;; exponent-marker---an exponent marker.                        
;; decimal-digit---a digit in radix 10.                        
;; digit---a digit in the current input radix

(defvar *el-reader//pf/parse-float*
  (el-reader//ensure-complete-token
   (el-reader//parse-alt
    (el-reader//parse-seq
     (el-reader//parse-optional #'el-reader//parse-sign)
     (el-reader//parse-kleene-star
      #'el-reader//parse-decimal-digit)
     #'el-reader//parse-decimal-point
     (el-reader//parse-plus #'el-reader//parse-decimal-digit)
     (el-reader//parse-optional #'el-reader//parse-exponent))
    (el-reader//parse-seq
     (el-reader//parse-optional #'el-reader//parse-sign)
     (el-reader//parse-plus #'el-reader//parse-decimal-digit)
     (el-reader//parse-optional
      (el-reader//parse-seq
       #'el-reader//parse-decimal-point
       (el-reader//parse-kleene-star
        #'el-reader//parse-decimal-digit)))
     #'el-reader//parse-exponent))))

(defvar *el-reader//parse-left-float*
  (el-reader//ensure-complete-token
   (el-reader//parse-seq
    (el-reader//parse-optional #'el-reader//parse-sign)
    (el-reader//parse-kleene-star #'el-reader//parse-decimal-digit)
    #'el-reader//parse-decimal-point
    (el-reader//parse-plus #'el-reader//parse-digit)
    (el-reader//parse-optional #'el-reader//parse-extension))))

(defvar *el-reader//parse-right-float*
  (el-reader//ensure-complete-token
   (el-reader//parse-seq
    (el-reader//parse-optional #'el-reader//parse-sign)
    (el-reader//parse-plus #'el-reader//parse-decimal-digit)
    (el-reader//parse-optional
     (el-reader//parse-seq
      #'el-reader//parse-decimal-point
      (el-reader//parse-kleene-star #'el-reader//parse-decimal-digit)))
    #'el-reader//parse-extension)))

;; (defvar *el-reader//parse-float*
;;   (el-reader//ensure-complete-token
;;    (el-reader//parse-alt *el-reader//parse-left-float*
;;                          *el-reader//parse-right-float*)))

(defun el-reader//parse-left-float (left)
  (pcase (slot-value left 'result)
    (`(,sign ,digits ,point ,mantissa ,exponent)
     (let ((sign-fn (el-reader//pf/sign-fn sign))
           (int-part (el-reader//pf/get-int-part digits))
           (point point)
           (mantissa (el-reader//pf/get-post-dec-places mantissa))
           (exponent (if exponent (slot-value exponent 'value) 0)))
       (setf (slot-value left 'result)
             (make-instance
              'el-reader//float
              :value (funcall sign-fn
                              (* (+ int-part mantissa)
                                 (expt 10 exponent)))))
       left))))

(defun el-reader//parse-right-float (right)
  (pcase (slot-value right 'result)
    (`(,sign ,digits ,point ,mantissa ,exponent)
     ;; In this case, we can just treat it as if it was a "left float".
     (el-reader//parse-left-float right))
    (`(,sign ,digits ,point-mantissa ,exponent)
     (let ((sign-fn (el-reader//pf/sign-fn sign))
           (int-part (el-reader//pf/get-int-part digits))
           (exponent (if exponent (slot-value exponent 'value) 0)))
       (setf (slot-value right 'result)
             (make-instance
              'el-reader//float
              :value (fancall sign-fn
                              (* int-part
                                 (expt 10 exponent)))))
       right))))

(defun el-reader//parse-float (token pos)
  (let* ((left? (funcall *el-reader//parse-left-float* token pos))
         (right? (if (slot-value left? 'success) nil
                   (funcall *el-reader//parse-right-float* token pos))))
    (cond ((slot-value left? 'success) (el-reader//parse-left-float left?))
          ((slot-value right? 'success) (el-reader//parse-right-float right?))
          (t right?)))
  ;; (el-reader//parse-float-el token pos)
  )

;; (defvar *el-reader//pf/integer*
;;   (el-reader//parse-alt
;;    (el-reader//parse-seq
;;     (el-reader//parse-optional #'el-reader//parse-sign)
;;     (el-reader//parse-plus #'el-reader//parse-decimal-digit)
;;     #'el-reader//parse-decimal-point)
;;    (el-reader//parse-seq
;;     (el-reader//parse-optional #'el-reader//parse-sign)
;;     (el-reader//parse-plus #'el-reader//parse-digit))))

(defvar *el-reader//pf/parse-exponent*
  (el-reader//parse-seq
   #'el-reader//parse-exponent-marker
   (el-reader//parse-optional #'el-reader//parse-sign)
   (el-reader//parse-plus #'el-reader//parse-digit)))

(defun el-reader//pf/sign-fn (sign)
  (pcase sign
    ((and (pred el-reader//sign-p)
          (guard (eq (slot-value sign 'value) 'minus-sign)))
     #'-)
    (_ #'identity)))

(defun el-reader//parse-exponent (token pos)
  (let ((res (funcall *el-reader//pf/parse-exponent* token pos)))
    (when (slot-value res 'success)
      (setf (slot-value res 'result)
            (make-instance
             'el-reader//exponent
             :value
             (pcase (slot-value res 'result)
               (`(,_ ,sign ,digits)
                (funcall (el-reader//pf/sign-fn sign)
                         (el-reader//digit-list->int
                          (el-reader//drop-leading-zeros digits))))))))
    res))

(defvar *el-reader//pf/parse-inf-marker*
  (el-reader//parse-seq
   (el-reader//parse-single-char ?+)
   (el-reader//parse-single-char ?I)
   (el-reader//parse-single-char ?N)
   (el-reader//parse-single-char ?F)))

(defun el-reader//parse-inf-marker (token pos)
  (let ((res (funcall *el-reader//pf/parse-inf-marker* token pos)))
    (when (slot-value res 'success)
      (setf (slot-value res 'result)
            (make-instance
             'el-reader//inf-marker
             :value 'inf)))
    res))

(defvar *el-reader//pf/parse-nan-marker*
  (el-reader//parse-seq
   (el-reader//parse-single-char ?+)
   (el-reader//parse-single-char ?N)
   (el-reader//parse-single-char ?a)
   (el-reader//parse-single-char ?N)))

(defun el-reader//parse-nan-marker (token pos)
  (let ((res (funcall *el-reader//pf/parse-nan-marker* token pos)))
    (when (slot-value res 'success)
      (setf (slot-value res 'result)
            (make-instance
             'el-reader//nan-marker
             :value 'nan)))
    res))

(defvar *el-reader//pf/parse-extension*
  (el-reader//parse-alt
   #'el-reader//parse-exponent
   (el-reader//parse-seq
    #'el-reader//parse-exponent-marker
    (el-reader//parse-alt
     #'el-reader//parse-inf-marker
     #'el-reader//parse-nan-marker))))

(defun el-reader//parse-extension (token pos)
  (let* ((res (funcall *el-reader//pf/parse-extension* token pos))
         (res-result (slot-value res 'result)))
    (setf
     (slot-value res 'result)
     (pcase res-result
       ((and ext
             (pred el-reader//exponent-p))
        res-result)
       ((and `(,exp-marker ,inf-marker)
             (guard (and (el-reader//exponent-marker-p exp-marker)
                         (el-reader//inf-marker-p inf-marker))))
        inf-marker)
       ((and `(,exp-marker ,nan-marker)
             (guard (and (el-reader//exponent-marker-p exp-marker)
                         (el-reader//nan-marker-p nan-marker))))
        nan-marker)))
    res))

;; (setf *el-reader//pf/parse-float*
;;       (el-reader//ensure-complete-token
;;        (el-)))

;; (defun el-reader//parse-float-el (token pos)
;;   (let* ((flt (funcall *el-reader//pf/parse-float* token pos)))
;;     ;;; NB: “Left” refers to the grammar given here:
;;     ;;; http://www.lispworks.com/documentation/lw70/CLHS/Body/02_ca.htm Left
;;     ;;; mereley means that it is the first case, rather than the latter.  This
;;     ;;; is why the extraction of elements of the parse tree is slightly
;;     ;;; different for the two cases.
;;     (if (slot-value flt 'success)
;;         (clone
;;          flt
;;          :result
;;          (make-instance
;;           'el-reader//syntax-element
;;           :value (el-reader//pf/fe (slot-value flt 'result))))
;;       flt)))

(defun el-reader//parse-float-el (token pos)
  (let* ((flt (funcall *el-reader//pf/parse-float* token pos)))
    ;;; NB: “Left” refers to the grammar given here:
    ;;; http://www.lispworks.com/documentation/lw70/CLHS/Body/02_ca.htm Left
    ;;; mereley means that it is the first case, rather than the latter.  This
    ;;; is why the extraction of elements of the parse tree is slightly
    ;;; different for the two cases.
    (when (slot-value flt 'success)
      (setf
       (slot-value flt 'result)
       (make-instance
        'el-reader//syntax-element
        :value (el-reader//pf/fe (slot-value flt 'result)))))
    flt))

;; (defun el-reader//parse-float (token pos)
;;   (el-reader//parse-float-el token pos))

(defvar *el-reader//pi/parse-integer*
      (el-reader//ensure-complete-token
       (el-reader//parse-alt
        (el-reader//parse-seq
         (el-reader//parse-optional #'el-reader//parse-sign)
         (el-reader//parse-plus #'el-reader//parse-decimal-digit)
         #'el-reader//parse-decimal-point)
        (el-reader//parse-seq
         (el-reader//parse-optional #'el-reader//parse-sign)
         (el-reader//parse-plus #'el-reader//parse-digit)))))

;; (defun el-reader//parse-integer (token pos)
;;   (let ((int (funcall *el-reader//pi/parse-integer* token pos)
;;          ;; (funcall
;;          ;;  (el-reader//ensure-complete-token
;;          ;;   (el-reader//parse-alt
;;          ;;    (el-reader//parse-seq
;;          ;;     (el-reader//parse-optional #'el-reader//parse-sign)
;;          ;;     (el-reader//parse-plus #'el-reader//parse-decimal-digit)
;;          ;;     #'el-reader//parse-decimal-point)
;;          ;;    (el-reader//parse-seq
;;          ;;     (el-reader//parse-optional #'el-reader//parse-sign)
;;          ;;     (el-reader//parse-plus #'el-reader//parse-digit))))
;;          ;;  token pos)
;;          ))
;;     (if (slot-value int 'success)
;;         (clone
;;          int
;;          :result
;;          (make-instance
;;           'el-reader//syntax-element
;;           :value (el-reader//make-int (slot-value int 'result))))
;;       int)))

(defun el-reader//parse-integer (token pos)
  (let ((int (funcall *el-reader//pi/parse-integer* token pos)
         ;; (funcall
         ;;  (el-reader//ensure-complete-token
         ;;   (el-reader//parse-alt
         ;;    (el-reader//parse-seq
         ;;     (el-reader//parse-optional #'el-reader//parse-sign)
         ;;     (el-reader//parse-plus #'el-reader//parse-decimal-digit)
         ;;     #'el-reader//parse-decimal-point)
         ;;    (el-reader//parse-seq
         ;;     (el-reader//parse-optional #'el-reader//parse-sign)
         ;;     (el-reader//parse-plus #'el-reader//parse-digit))))
         ;;  token pos)
         ))
    (when (slot-value int 'success)
      (setf
       (slot-value int 'result)
       (make-instance
        'el-reader//syntax-element
        :value (el-reader//make-int (slot-value int 'result)))))
    int))

(defvar *el-reader//parse-numeric-token*
      (el-reader//parse-alt #'el-reader//parse-integer
                            #'el-reader//parse-float))

;; (defun el-reader//parse-numeric-token (token pos)
;;   (funcall
;;    (el-reader//parse-alt #'el-reader//parse-integer
;;                         #'el-reader//parse-float)
;;    token pos))

(defun el-reader//map-string-as-substrings (fn token)
  "Works like `mapcar' on a string, yet passes the chars as singleton strings,
leaving the properties intact.  The result is a list of the results, in order."
  (let ((res nil))
    (dotimes (i (length token))
      (push (funcall fn (substring token i (1+ i)))
            res))
    (seq-reverse res)))

(define-error 'reader-error "The reader encountered an error")

(defun el-reader//parse-symbol (token pos)
  (when (not (zerop pos))
    (warn "POS is nonzero, this probably shouldn’t happen!"))
  (cl-labels ((escaped-p
               (str pos)
               (get-text-property pos 'escapedp str)))
    (let ((name (substring-no-properties token pos)))
      (if (and (string= name ".")
               (not (get-text-property pos 'escapedp token))
               (not *el-reader//allow-single-dot-symbol*))
          (signal 'invalid-read-syntax "invalid-read-syntax: \".\"")
        (intern name)))))

;; TODO: possibly build in package support.  This would need a hook of some
;; sort.  Currently, package support is impossible, as a symbol may not be
;; interned into more than one obarry at a time.  This is because in the
;; underlying C implementation, a symbol itself contains a next pointer to the
;; next symbol in the obarray (instead of having the symbols in a list).  This
;; makes symbol lookup slightly faster, at the expense of having no proper
;; package support.  Possibly the emacs-devel team can be convinced to change
;; this.

(defun el-reader//contains-non-number-syntax-type? (token)
  (not
   (-all?
    #'identity
    (el-reader//map-string-as-substrings
     (lambda (c)
       (and
        (not (get-text-property 0 'escapedp c))
        (-any? #'identity
               (cons
                (-when-let
                    (d? (gethash
                         (string-to-char c)
                         (el-reader//rt/char-to-num *el-reader/readtable*)))
                  (< d? *el-reader/read-base*))
                (seq-map (-partial #'seq-contains
                                   (get-text-property 0 'traits c))
                         '(plus-sign
                           minus-sign
                           ratio-marker
                           decimal-point
                           dot
                           float-exponent-marker
                           double-float-exponent-marker
                           single-float-exponent-marker
                           long-float-exponent-marker
                           short-float-exponent-marker))))))
     token))))

(defun el-reader//process-token (token)
  ;; (setf *el-reader//debug-stuff* (funcall ))
  (let ((num? (funcall *el-reader//parse-numeric-token* token 0)))
    (if (slot-value num? 'success)
        (slot-value (slot-value num? 'result) 'value)
      (el-reader//parse-symbol token 0))))

;; (defun el-reader//process-token (token)
;;   (advice-remove 'read #'read@el-reader//replace-read)
;;   (unwind-protect
;;       (let ((r (read (substring-no-properties token))))
;;         (if (numberp r)
;;             r
;;           (el-reader//parse-symbol token 0)))
;;     (advice-add 'read :around #'read@el-reader//replace-read)))

;; (defun el-reader//process-token (token)
;;   (advice-remove 'read #'read@el-reader//replace-read)
;;   (unwind-protect
;;       (read (substring-no-properties token))
;;     (advice-add 'read :around #'read@el-reader//replace-read)))

(defun el-reader//token->escaped-str (token)
  (let* ((i 0)
         (j 0)
         (len (length token))
         (newtok (make-vector (* 2 len) 0)))
    (while (< i len)
      (when (get-text-property i 'escapedp token)
        (setf (aref newtok j) ?\\)
        (incf j))
      ;; (push (aref token i) newtok)
      (setf (aref newtok j) (aref token i))
      (incf i)
      (incf j))
    (substring (concat newtok) 0 j)))

;; This was used recently.  Works fine, unless a base != 10 is used :/

;; (defun el-reader//process-token (token)
;;   ;; (el-reader//contains-non-number-syntax-type? token)
;;   (if (text-property-any 0 (length token) 'escapedp t token)
;;       ;; (-any? #'identity
;;       ;;        (el-reader//map-string-as-substrings
;;       ;;         (lambda (c) (get-text-property 0 'escapedp c)) token))
;;       (el-reader//parse-symbol token 0)
;;     (advice-remove 'read #'read@el-reader//replace-read)
;;     (unwind-protect
;;         (read (substring-no-properties token))
;;       (advice-add 'read :around #'read@el-reader//replace-read))))

;; (defun el-reader//process-token (token)
;;   (advice-remove 'read #'read@el-reader//replace-read)
;;   (unwind-protect
;;       (read (substring-no-properties
;;              (if (text-property-any 0 (length token) 'escapedp t token)
;;                  (el-reader//token->escaped-str token)
;;                token)))
;;     (advice-add 'read :around #'read@el-reader//replace-read)))

(defun el-reader/read-delimited-list (char &optional stream _recursive-p)
  (let ((end-res *el-reader/repeat-read*))
    (while (eq end-res *el-reader/repeat-read*)
      (setf
       end-res
       (let ((res-list nil)
             (c (el-reader/peek-char stream)))
         (while (/= c char)
           (cond
            ((el-reader//rt/invalid-syntax-type-p *el-reader/readtable* c)
             (signal 'reader-error (list "Invalid char" c)))
            ((el-reader//rt/whitespacep *el-reader/readtable* c)
             (el-reader/getch stream)
             (setf c (el-reader/peek-char stream)))
            ((or (el-reader//rt/constituentp *el-reader/readtable* c)
                 (el-reader//rt/terminating-macro-char-p
                  *el-reader/readtable* c)
                 (el-reader//rt/non-terminating-macro-char-p
                  *el-reader/readtable* c)
                 (el-reader//rt/single-escape-char-p *el-reader/readtable* c)
                 (el-reader//rt/multiple-escape-char-p *el-reader/readtable* c))
             (let ((res (el-reader/read stream t nil t '(:return-list t))))
               (if res
                   (if (listp res)
                       (push (car res) res-list)
                     (error (s-join " " '("Read-macro functions must always "
                                          "return a (possibly empty) list."))))
                 *el-reader/repeat-read*))
             (setf c (el-reader/peek-char stream)))))
         (el-reader/getch stream)
         (reverse res-list))))
    end-res))

(cl-defun el-reader/make-dispatching-function ()
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

;; This function is not used by default.  It may be used to set arbitrary chains
;; of macro chars, not just 2 (char and sub-char).

(defun el-reader/set-dispatch-macro-function (disp-fun sub-char new-function
                                                        &optional
                                                        non-terminating-p)
  (funcall disp-fun nil nil :set sub-char new-function non-terminating-p))

(defun el-reader//read-decimal (stream)
  (cl-flet ((between (x a b) (if (and (>= x a) (<= x b))
                              x
                            nil))) 
   (do ((c (el-reader/getch stream) (el-reader/getch stream))
        (res nil (if (between c ?0 ?9) (+ (* (or res 0) 10) (- c ?0)) res)))
       ((not (between c ?0 ?9)) (el-reader/getch stream c) res))))

(cl-defun el-reader/make-dispatch-macro-character
    (char &optional non-terminating-p (readtable *el-reader/readtable*))
  (when (and (el-reader//memhash
              char
              (el-reader//rt/classic-dispatch-functions readtable))
             (hash-table-p
              (gethash char
                       (el-reader//rt/classic-dispatch-functions readtable))))
    (setf (gethash char (el-reader//rt/classic-dispatch-functions readtable))
          (make-hash-table)))
  
  (el-reader/set-macro-character
   char
  (lambda (stream char)
     (let* ((n (el-reader//read-decimal stream))
            (c (el-reader/getch stream))
            (f (gethash
                c
                (gethash
                 char
                 (el-reader//rt/classic-dispatch-functions readtable)
                 (make-hash-table)))))
       (condition-case nil (funcall f stream c n)
         (t (error "Invalid read syntax: \"%c\"" char)))))
   non-terminating-p readtable))

(cl-defun el-reader/get-dispatch-macro-character
    (disp-char sub-char &optional (readtable *el-reader/readtable*))
  (gethash sub-char
           (or
            (gethash disp-char
                     (el-reader//rt/classic-dispatch-functions readtable))
            (make-hash-table))))

(cl-defun el-reader/set-dispatch-macro-character
    (disp-char sub-char new-function &optional (readtable *el-reader/readtable*))
  (when (not (hash-table-p
              (gethash disp-char
                       (el-reader//rt/classic-dispatch-functions readtable))))
    (setf (gethash disp-char
                   (el-reader//rt/classic-dispatch-functions readtable))
          (make-hash-table)))
  (setf (gethash sub-char
                 (gethash disp-char
                          (el-reader//rt/classic-dispatch-functions readtable)))
        new-function)
  t)

(cl-defun el-reader//put-escaped-prop (s &optional (from 0) (to (length s)))
  (put-text-property
   from to 'escapedp t s)
  s)

(defun el-reader//has-case-p (c)
  (not (= (upcase c) (downcase c))))

(defun el-reader//force-alphabetic (c)
  (let ((z (char-to-string c)))
    (put-text-property 0 1 'syntax-type 'constituent z)
    (put-text-property 0 1 'traits '(alphabetic) z)
    z))

(defun el-reader//switch-case (c)
  (if (= (upcase c) c)
      (downcase c)
    (upcase c)))

(cl-defun el-reader//step-8 (input-stream token _x)
  ;; Loop until we do an explicit return.  This would have been so much
  ;; nicer with tco.  *sigh*
  (let (y)
    (while t
      (setf y (condition-case nil
                  (el-reader/getch input-stream)
                (end-of-file (cl-return-from el-reader//step-8
                               (el-reader//process-token token)))))
      (cond ((el-reader//rt/invalid-syntax-type-p *el-reader/readtable* y)
             (signal 'reader-error (list "Invalid character" y)))
            ((el-reader//rt/single-escape-char-p
              *el-reader/readtable* y)
             (setf token (s-concat token
                                   (el-reader//put-escaped-prop
                                    (el-reader//force-alphabetic
                                     (el-reader/getch input-stream))))))
            ((el-reader//rt/multiple-escape-char-p
              *el-reader/readtable* y)
             (cl-return-from el-reader//step-8
               (el-reader//step-9 input-stream
                                  token
                                  y)))
            ((el-reader//rt/terminating-macro-char-p
              *el-reader/readtable* y)
             (el-reader/getch input-stream y)
             (cl-return-from el-reader//step-8
               (el-reader//process-token token)))
            ((el-reader//rt/whitespacep *el-reader/readtable* y)
             (when *el-reader/preserve-whitespace*
               (el-reader/getch input-stream y))
             (cl-return-from el-reader//step-8
               (el-reader//process-token token)))
            ((or (el-reader//rt/constituentp *el-reader/readtable* y)
                 (el-reader//rt/non-terminating-macro-char-p
                  *el-reader/readtable* y))
             (setf token (s-concat token
                                   (el-reader//defaults-to-str-props y))))))))

(cl-defun el-reader//step-9 (input-stream token x)
  ;; At this point a token is being accumulated, and an odd
  ;; number of multiple escape characters have been encountered.
  (let (y)
    (while t
      (setf y (el-reader/getch input-stream))
      (cond
       ((funcall (apply
                  #'-orfn
                  (seq-map
                   (lambda (fn)
                     (-partial fn *el-reader/readtable*))
                   (list
                    #'el-reader//rt/constituentp
                    #'el-reader//rt/terminating-macro-char-p
                    #'el-reader//rt/non-terminating-macro-char-p
                    #'el-reader//rt/whitespacep)))
                 y)
        (setf token (s-concat token (el-reader//put-escaped-prop
                                     (el-reader//force-alphabetic y)))))
       ((el-reader//rt/single-escape-char-p *el-reader/readtable* y)
        (setf token (s-concat
                     token
                     (el-reader//put-escaped-prop
                      (el-reader//force-alphabetic
                       (el-reader/getch input-stream))))))
       ((el-reader//rt/multiple-escape-char-p *el-reader/readtable* y)
        (cl-return-from el-reader//step-9
          (el-reader//step-8 input-stream token x)))
       ((el-reader//rt/invalidp *el-reader/readtable* y)
        (signal 'reader-error "Invalid char"))))))

(cl-defun el-reader/read (&optional input-stream (eof-error-p t) eof-value
                                    recursive-p
                                    keys)
  (let ((res *el-reader/repeat-read*)
        (input-stream (el-reader//get-getch-state input-stream)))
    (while (eq res *el-reader/repeat-read*)
      (setf
       res
       (let ((x (condition-case c
                    (el-reader/getch input-stream)
                  (end-of-file
                   (if eof-error-p
                       (signal (car c) (cdr c))
                     (cl-return-from el-reader/read eof-value))))))
         (when (not recursive-p)
           (setf *el-reader//read-objects* nil
                 ;; *el-reader//circular-read-functions* nil
                 ))
         (cond ((el-reader//rt/invalid-syntax-type-p *el-reader/readtable* x)
                (signal 'reader-error (list "Invalid char" x)))
               ((el-reader//rt/whitespacep *el-reader/readtable* x)
                *el-reader/repeat-read*)
               ((el-reader//rt/terminating-macro-char-p
                 *el-reader/readtable* x)
                (let ((*el-reader//allow-single-dot-symbol* nil))
                  (let ((res (funcall (gethash x (el-reader//rt/term-mac-fns
                                                  *el-reader/readtable*))
                                      input-stream x)))
                    (when (not (listp res))
                      (error (s-join " " '("Read-macro functions must always"
                                           "return a (possibly empty) list."))))
                    res)))
               ((el-reader//rt/non-terminating-macro-char-p
                 *el-reader/readtable* x)
                (let ((*el-reader//allow-single-dot-symbol* nil))
                  (let ((res (funcall (gethash x (el-reader//rt/non-term-mac-fns
                                                  *el-reader/readtable*))
                                      input-stream x)))
                    (when (not (listp res))
                      (error (s-join " " '("Read-macro functions must always"
                                           "return a (possibly empty) list."))))
                    res)))
               ((el-reader//rt/single-escape-char-p *el-reader/readtable* x)
                (cl-values (el-reader//step-8
                            input-stream
                            (el-reader//put-escaped-prop
                             (el-reader//force-alphabetic
                              (el-reader/getch input-stream)))
                            x)))
               ((el-reader//rt/multiple-escape-char-p *el-reader/readtable* x)
                (cl-values (el-reader//step-9 input-stream "" x)))
               ((el-reader//rt/constituentp *el-reader/readtable* x)
                (cl-values (el-reader//step-8 input-stream
                                              (el-reader//defaults-to-str-props x)
                                              x)))
               (t (error "PANIC!!! THIS SHOULD NEVER HAVE HAPPENED!!!"))))))
    (let ((ret-val
           (if (plist-get keys :return-list)
               res
             (if res
                 (car res)
               (let ((res))
                 (while (not res)
                   (setf res
                         (el-reader/read
                          input-stream
                          eof-error-p
                          eof-value
                          recursive-p
                          '(:return-list t))))
                 (car res))))))
      (if (not recursive-p)
          (el-reader//replace-placeholders ret-val)
        ret-val))))

(cl-defun el-reader/read-preserving-whitespace (&optional input-stream
                                                          (eof-error-p t)
                                                          eof-value recursive-p)
  (if recursive-p
      (el-reader/read input-stream eof-error-p eof-value recursive-p)
    (let ((*el-reader/preserve-whitespace* t))
      (el-reader/read input-stream eof-error-p eof-value recursive-p))))

;; Now that we have defined the mechanism, it is time to define our macros, so
;; we can read something other than symbols and numbers :)

;; (defun el-reader//read-lisp-list (stream _char)
;;   (cl-values
;;    (let ((*el-reader//allow-single-dot-symbol* t))
;;      (let ((l (el-reader/read-delimited-list ?\) stream t)))
;;        (let ((l- (if (and (>= (seq-length l) 3)
;;                           (eq (seq-elt l (- (length l) 2)) (intern ".")))
;;                      (append (seq-subseq l 0 (- (seq-length l) 2))
;;                              (seq-elt l (1- (seq-length l))))
;;                    l)))
;;          (if (not (cl-loop for s in l- if (eq s (intern ".")) collect s))
;;              (prog1 l-
;;                (unintern "." obarray))
;;            (unintern "." obarray)
;;            (signal 'reader-error "invalid-read-syntax: \".\"")))))))

(defun el-reader//read-lisp-list (stream _char)
  (cl-values
   (let ((*el-reader//allow-single-dot-symbol* t))
     (let ((l (el-reader/read-delimited-list ?\) stream t))
           (dot (intern ".")))
       (cond ((and (>= (seq-length l) 3)
                   (eq (seq-elt l (- (seq-length l) 2)) dot)
                   (= 1 (cl-loop for c = 0 for s in l if (eq s dot) count c)))
              (append (seq-subseq l 0 (- (seq-length l) 2))
                             (seq-elt l (1- (seq-length l))))
              ;; (let ((lr (nreverse l)))
              ;;   (nreverse (cons (car lr) (cddr lr))))
              )
             ((not (zerop (cl-loop for c = 0 for s in l if (eq s dot) count c)))
              (unintern "." obarray)
              (signal 'reader-error "invalid-read-syntax: \".\""))
             (t
              l))))))

(defun el-reader//read-comment (stream _char)
  (cl-do ((c (el-reader/peek-char stream)
             (progn
               (el-reader/getch stream)
               (el-reader/peek-char stream))))
      ((= c ?\n) (progn
                   (el-reader/getch stream)
                   (cl-values)))))

(defun el-reader//read-vector (stream _char)
  (cl-values (apply #'vector (el-reader/read-delimited-list ?\] stream t))))

;; Should we ever come across a closing paren, we know it is unbalanced, as
;; read-demilited-list consumes the closing character.

(defun el-reader//read-lone-close-paren (&rest _args)
  (signal 'unbalanced-sexp nil))

(el-reader/set-macro-character ?\) #'el-reader//read-lone-close-paren)

;; Make ] do the same thing as ), namely signal an error.

(cl-multiple-value-bind (fun _term)
    (el-reader/get-macro-character ?\))
  (el-reader/set-macro-character ?\] fun))

(el-reader/set-macro-character ?\( #'el-reader//read-lisp-list)

(el-reader/set-macro-character ?\" #'el-reader/read-string)

(el-reader/set-macro-character ?? #'el-reader/read-char t)

(el-reader/set-macro-character ?\[ #'el-reader//read-vector)

(defun el-reader//read-quote (stream _char)
  (cl-values `',(el-reader/read stream t nil t)))

(defun el-reader//read-backquote (stream _char)
      (cl-values `(,(intern "`") ,(el-reader/read stream t nil t))))

(defun el-reader//read-comma (stream _char)
      (cl-values
       (let ((next (el-reader/peek-char stream)))
         (if (/= ?@ next)
             `(,(intern ",") ,(el-reader/read stream t nil t))
           (el-reader/getch stream)
           `(,(intern ",@") ,(el-reader/read stream t nil t))))))

(el-reader/set-macro-character ?\' #'el-reader//read-quote)

(el-reader/set-macro-character ?, #'el-reader//read-comma)

(el-reader/set-macro-character ?` #'el-reader//read-backquote)

(el-reader/set-macro-character ?\; #'el-reader//read-comment)

(defun el-reader//read-int-radix (stream _char radix)
   (let ((*el-reader/read-base* radix))
     (let ((r (el-reader/read stream)))
       (if (integerp r)
           (cl-values r)
         (error "invalid-read-syntax \"integer, radix %i\"" radix)))))

(el-reader/set-dispatch-macro-character ?# ?r #'el-reader//read-int-radix)

(cl-multiple-value-bind (fun _term)
    (el-reader/get-macro-character ?\))
  (el-reader/set-macro-character ?\} fun))

(defun el-reader//read-hash-table (stream _char)
   (cl-values
    (let ((k-v (el-reader/read-delimited-list ?\} stream t)))
      (if (= (mod (length k-v) 2) 1)
          (error "Invalid syntax: {}")
        `(el-reader//ht ,@k-v)))))

(el-reader/set-macro-character ?\{ #'el-reader//read-hash-table)
;; While # is a non-terminating char in CL, el has no such thing, so we won’t
;; make it non-terminating.

(el-reader/make-dispatch-macro-character ?# nil)

(defun el-reader//read-function-quote (stream _char _number)
   (cl-values `(function ,(el-reader/read stream t nil t))))

(el-reader/set-dispatch-macro-character ?# ?' #'el-reader//read-function-quote)

;; (defun el-reader//read-= (stream _char number)
;; ;;; save a placeholder (these are all unique)
;;   (warn
;;    "%s"
;;    "Ummmm ... this function seems to be a little weird, is it still called?")
;;   (let ((placeholder (cons nil nil))) 
;;     ;; (setf (gethash number *el-reader//read-objects*)
;;     ;;       placeholder)
;;     (push (cons number placeholder) *el-reader//read-objects*)
;;     (let ((obj (el-reader/read stream t nil t)))
;;       (push (lambda (r)
;;               (el-reader//replace-placeholder r placeholder obj)
;;               r)
;;             *el-reader//circular-read-functions*)
;;       obj)))

(defun el-reader//deep-follow-replacement (obj)
  (do ((repl (list (el-reader//get-placeholder-replacement obj))
             (cons (el-reader//get-placeholder-replacement (car repl))
                   repl)))
      ((null (car repl)) (second repl))))

(defun el-reader//replace-placeholders (obj)
  "Walks OBJ, replaces all placeholders in the global placeholder list."
  (cond ((consp obj)
         (let ((repl (el-reader//deep-follow-replacement obj)))
           (if repl
               repl
             (setf (car obj) (el-reader//replace-placeholders (car obj))
                   (cdr obj) (el-reader//replace-placeholders (cdr obj)))
             obj)))
        ((and (arrayp obj)
              (not (stringp obj)))
         (dotimes (i (length obj))
           (aset obj i (el-reader//replace-placeholders (aref obj i))))
         obj)
        ((hash-table-p obj)
         (seq-do
          (lambda (k)
            (let ((v (gethash k obj)))
              (remhash k obj)
              (setf (gethash (el-reader//replace-placeholders k) obj)
                    (el-reader//replace-placeholders v))))
          (hash-table-keys obj))
         obj)
        ((atom obj) obj
         ;; (let ((repl (el-reader//deep-follow-replacement obj)))
         ;;   (if (null repl)
         ;;       obj
         ;;     repl))
         )))

(defun el-reader//num->placeholder (n)
  (cl-loop for triple in *el-reader//read-objects*
           if (= (seq-elt triple 0) n) return (seq-elt triple 1)))

(defun el-reader//potential-placeholder-p (obj)
  (and (consp obj)
       (null (car obj))
       (null (cdr obj))))

(defun el-reader//get-placeholder-replacement (placeholder)
  "Returns the object which shall replace PLACEHOLDER, or nil."
  (cl-loop for triple in *el-reader//read-objects*
           if (eq (seq-elt triple 1) placeholder) return (seq-elt triple 2)))

(defun el-reader//set-placeholder (placeholder obj)
  (seq-map (lambda (triple)
             (when (eq (seq-elt triple 1) placeholder)
               (setf (seq-elt triple 2) obj))
             triple)
           *el-reader//read-objects*))

(defun el-reader//read-= (stream _char number)
;;; save a placeholder (these are all unique)
  (let ((placeholder (cons nil nil)))
    ;; push a triple of the NUMBER, the PLACEHOLDER object and nil onto the
    ;; global list.  NIL shall later be replaced by the object which shall
    ;; replace PLACEHOLDER.
    (push (vector number placeholder nil) *el-reader//read-objects*)
    (let ((obj (el-reader/read stream t nil t)))
      (el-reader//set-placeholder placeholder obj)
      (cl-values obj))))

(el-reader/set-dispatch-macro-character ?# ?= #'el-reader//read-=)

(defun el-reader//read-hash-num-hash (_stream _char number)
  (if (not number)
      (intern "")                       ; empty symbol
    (let ((placeholder (el-reader//num->placeholder number)))
      (if (not (el-reader//potential-placeholder-p placeholder))
          (signal 'invalid-read-syntax (format "#%s#" number))
        (cl-values placeholder)))))

(el-reader/set-dispatch-macro-character ?# ?# #'el-reader//read-hash-num-hash)

(defun el-reader//read-hash< (_stread _char _number)
  (signal 'invalid-read-syntax "Invalid read syntax #"))

(el-reader/set-dispatch-macro-character ?# ?< #'el-reader//read-hash<)

(defun el-reader//read-char-table (_stream _char _number)
  ;; Syntax:
  ;; - The first element is the default value
  ;; - The second element of #^[...] is the parent.
  ;; - The third element is the type.
  ;; - The char-table-extra-slots property of the subtype specifies the number
  ;;   of extra slots.
  ;; - 
  )

(defun el-reader//read-byte-code (stream _char _number)
  (let ((v (el-reader/read-delimited-list ?\] stream t)))
    (list (apply #'make-byte-code (map 'list #'identity v)))))

(el-reader/set-dispatch-macro-character ?# ?\[ #'el-reader//read-byte-code)

;; (define-advice el-reader/read
;;     (:after (&optional _input-stream _eof-error-p _eof-value recursive-p
;;                        &rest _ignore)
;;             nil
;;             -100)
;;   (when (not recursive-p)
;;     ;; (dolist (f *el-reader//circular-read-advice*)
;;     ;;   (funcall f))
;;     (setf *el-reader//circular-read-functions* nil)))

;; (define-advice el-reader/read (:filter-return (r))
;;   (funcall (apply #'-compose *el-reader//circular-read-functions*) r))

(define-advice read (:around (oldfun &optional stream) el-reader//replace-read)
  ;; Don’t use this if we’re reading bytecode.  Emacs elegantly uses its reader
  ;; to read bytecode, but they use weird read-macros which have not been
  ;; implemented. Also they use a weird function called `get-file-char', which
  ;; does not take an optional argument.

  (if (and use-el-reader (not el-reader-bytecode))
      (el-reader/read stream)
    (funcall oldfun stream)))

(provide 'el-reader)

;;; el-reader.el ends here
