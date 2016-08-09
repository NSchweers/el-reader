;; (require 'el-reader)

(require 'f)

(ert-deftest elr-test/simple-symbol ()
  "Read the symbol \"foo\" and check if it’s `eq' to what Emacs emits."
  :tags '(read-symbol)
  (should (eq (el-reader/read "foo") 'foo)))

(ert-deftest elr-test/single-escaped-symbol ()
  "Read a symbol which contains single escapes"
  :tags '(read-symbol)
  (should (eq (el-reader/read "\\foo") '\foo)))

(ert-deftest elr-test/single-escaped-symbol-paren ()
  "Read a symbol containing single escaped parens"
  :tags '(read-symbol)
  (should (eq (el-reader/read "\\(foo\\)") '\(foo\))))

(ert-deftest elr-test/list ()
  "Reads a list of symbols"
  :tags '(read-list read-symbol)
  (should (tree-equal
           (el-reader/read "(foo bar spam)")
           '(foo bar spam)
           :test #'eq)))

(ert-deftest elr-test/decimal ()
  "Reads decimal numbers"
  :tags '(read-dec read-num)
  (should (= (el-reader/read "2") 2)))

(ert-deftest elr-test/random-decimals ()
  "Reads random decimals.

How many random numbers are read is taken from *erl-test/read-decimal-count*"
  :tags '(read-dec read-num expensive)
  (dotimes (i *elr-test/read-decimal-count*)
    (should (let ((rn (random)))
              (= (el-reader/read (prin1-to-string rn))
                 rn)))))

(ert-deftest elr-test/dotted-pair ()
  "Reads a dotted pair"
  :tags '(read-symbol read-cons read-dot)
  (let ((c (el-reader/read "(foo . bar)")))
    (should (consp c))
    (should (eq (car c) 'foo))
    (should (eq (cdr c) 'bar))))

(ert-deftest elr-test/dotted-pair-with-list ()
  "Reads a dotted pair with a list in tail position"
  :tags '(read-symbol read-cons read-dot read-list)
  (let* ((str "(a . (b c))")
         (elr-dp (el-reader/read str))
         (el-dp (read str)))
    (should-not (seq-filter #'identity (seq-mapn (-compose #'null #'eq) elr-dp el-dp)))))

(ert-deftest elr-test/empty-list ()
  "Reads the empty list"
  :tags '(read-list nil empty)
  (should (null (el-reader/read "()"))))

(ert-deftest elr-test/improper-list ()
  "Reads an improper list"
  :tags '(read-symbol read-cons read-dot read-list improper-list)
  (let ((c (el-reader/read "(foo bar . spam)")))
    (should (consp c))
    (should (consp (cdr c)))
    (should (eq (car c) 'foo))
    (should (eq (cadr c) 'bar))
    (should (eq (cddr c) 'spam))))

(ert-deftest elr-test/broken-improper-list ()
  "Should fail on a broken improper list"
  :tags '(shall-fail read-list read-dot read-cons improper-list read-symbol)
  (should-error (el-reader/read "(foo . bar . spam)")
                :type '(reader-error)))

(ert-deftest elr-test/proper-list ()
  "Reads a proper (normal) list"
  :tags '(read-num read-list)
  (let ((l (el-reader/read "(1 2 3)")))
    (should (equal l '(1 2 3)))))

(ert-deftest elr-test/singleton-list ()
  "Reads a singleton list"
  :tags '(read-list read-num)
  (let ((l (el-reader/read "(1)")))
    (should (equal l '(1)))))

(ert-deftest elr-test/empty-vector ()
  "Reads an empty vector"
  (let ((v (el-reader/read "[]")))
    (should (vectorp v))
    (should (zerop (length v)))))

(ert-deftest elr-test/singleton-vector ()
  "Reads a vector of one element"
  :tags '(read-vector read-num)
  (let ((v (el-reader/read "[1]")))
    (should (vectorp v))
    (should (= (length v) 1))
    (should (= (aref v 0) 1))))

(ert-deftest elr-test/2-vector ()
  "Reads a vector of length 2"
  :tags '(read-vector read-symbol)
  (let ((v (el-reader/read "[1 2]")))
    (should (vectorp v))
    (should (= (length v) 2))
    (should (= (aref v 0) 1))
    (should (= (aref v 1) 2))))

(ert-deftest elr-test/vector-correct-eval ()
  "Reads and evaluates a vector of symbols to check whether the contents are
evaluated or not."
  (let ((v (eval (el-reader/read "[foo bar]"))))
    (equal v [foo bar])))

(ert-deftest elr-test/vector-of-lists ()
  "Reads a vector of lists"
  :tags '(read-vector read-list read-num read-symbol nested)
  (let ((v (el-reader/read "[(1 2) (3 4 5) (foo bar spam and eggs)]")))
    (tree-equal v [(1 2) (3 4 5) (foo bar spam and eggs)])))

(ert-deftest elr-test/quote ()
  "Quotes a symbol"
  :tags '(read-symbol quote)
  (should (equal (el-reader/read "'foo") '(quote foo))))

(ert-deftest elr-test/simple-string ()
  "Reads a simple string"
  :tags '(read-string)
  (string= (el-reader/read "\"simple string\"") "simple-string"))

(ert-deftest elr-test/escaped-string ()
  "Reads a string containing escaped characters"
  :tags '(read-string str-escape)
  (string= (el-reader/read "\"string with an escaped\ttab\"")
           "string with an escaped\ttab"))

(ert-deftest elr-test/string-with-double-quotes ()
  "Reads a string containing escaped double quotes"
  :tags '(read-string str-escape double-quotes)
  (should (string= (el-reader/read "\"This string constains \\\"quotes\\\"\"")
                   "This string constains \"quotes\"")))

(ert-deftest elr-test/simple-float ()
  "Reads a simple floating point number"
  (let ((fs '("5." "1.5" "0.5" ".5")))
    (seq-do
     (lambda (s)
       (should (= (el-reader/read s) (read s))))
     fs)
    (should (integerp (el-reader/read (car fs))))
    (should (seq-every-p #'floatp (seq-map #'el-reader/read (cdr fs))))))

(ert-deftest elr-test/floatneg-zero ()
  "Reads negative zero"
  (should (= (el-reader/read "-0.0")
             (read "-0.0"))))

(ert-deftest elr-test/lispy-defvar ()
  "Reads a defvar from lispy.el which used to fail"
  (let ((str "(defvar lispy-parens-preceding-syntax-alist
  '((lisp-mode . (\"[#`',.@]+\" \"#[0-9]*\" \"#[.,Ss+-]\" \"#[0-9]+[=Aa]\"))
    (emacs-lisp-mode . (\"[#`',@]+\" \"#s\" \"#[0-9]+=\"))
    (clojure-mode . (\"[`'~@]+\" \"#\" \"#\\\\?@?\"))
    (clojurescript-mode . (\"[`'~@]+\" \"#\" \"#\\\\?@?\"))
    (clojurec-mode . (\"[`'~@]+\" \"#\" \"#\\\\?@?\"))
    (t . (\"[`',@]+\")))
  \"An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening paren in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-parens'.\")"))
    (should (tree-equal (read str)
                        (el-reader/read str)
                        :test #'equal))))

(ert-deftest elr-test/circular-object ()
  "Reads a circular object"
  (let* ((str "(#1=(a) b #1#)")
         (co (el-reader/read str)))
    (should (eq (nth 0 co)
                (nth 2 co)))
    (let* ((str "#1=(a #1#)")
           (co (el-reader/read str)))
      (should (eq co (second co))))))

(ert-deftest elr-internal/replace-empty-placeholders ()
  "Tests placeholder replacement in the empty global list."
  ;; Make a new list, don’t disrupt normal operation.
  (let ((*el-reader//read-objects* nil))
    (el-reader//set-placeholder (cons nil nil) t)
    (should (null *el-reader//read-objects*))))

(ert-deftest elr-internal/set-placeholder-singleton ()
  "Tests setting the sole placeholder in the global list."
  (let* ((*el-reader//read-objects* nil)
         (ph (cons nil nil)))
    (push (vector 1 ph nil) *el-reader//read-objects*)
    (el-reader//set-placeholder ph 'replaced)
    (should (= (seq-length *el-reader//read-objects*) 1))
    (should (eq (seq-elt (seq-elt *el-reader//read-objects* 0) 2)
                'replaced))))

(ert-deftest elr-internal/test-replace-placeholders-on-atom ()
  "Tests `el-reader//replace-placeholders' on atoms."
  (let ((*el-reader//read-objects* nil)
        ;; (p1 (cons nil nil))
        )
    (should (null (el-reader//replace-placeholders nil)))
    (should (eq 'sym (el-reader//replace-placeholders 'sym)))
    (should (= 1 (el-reader//replace-placeholders 1)))
    (should (eq (get-buffer "*scratch*")
                (el-reader//replace-placeholders (get-buffer "*scratch*"))))
    (should (let ((f "foo")) (eq f (el-reader//replace-placeholders f))))
    
    ;; (push (vector 1 p1 'error) *el-reader//read-objects*)

    (elr-test/with-objects
     (should (null (el-reader//replace-placeholders nil)))
     (should (eq 'sym (el-reader//replace-placeholders 'sym)))
     (should (= 1 (el-reader//replace-placeholders 1)))
     (should (eq (get-buffer "*scratch*")
                 (el-reader//replace-placeholders (get-buffer "*scratch*"))))
     (should (let ((f "foo")) (eq f (el-reader//replace-placeholders f)))))))

(ert-deftest elr-internal/test-replace-placeholders-on-cons ()
  "Tests `el-reader//replace-placeholders' on conses."
  (let ((*el-reader//read-objects* nil))
    (let ((o (cons 'x 'y)))
      (should (eq o (el-reader//replace-placeholders o)))
      (elr-test/with-objects
       (should (eq o (el-reader//replace-placeholders o)))))))

(ert-deftest elr-internal/test-replace-placeholders-on-list ()
  "Tests `el-reader//replace-placeholders' on conses."
  (let ((*el-reader//read-objects* nil))
    (let ((o (list 'x 'y)))
      (should (eq o (el-reader//replace-placeholders o)))
      (elr-test/with-objects
       (should (eq o (el-reader//replace-placeholders o)))))))

(ert-deftest elr-internal/test-replace-placeholders-on-vector ()
  "Tests `el-reader//replace-placeholders' on conses."
  (let ((*el-reader//read-objects* nil))
    (let ((o (vector 'x 'y)))
      (should (eq o (el-reader//replace-placeholders o)))
      (elr-test/with-objects
       (should (eq o (el-reader//replace-placeholders o)))))))

(ert-deftest elr-internal/test-replace-placeholders-on-hashmap ()
  "Tests `el-reader//replace-placeholders' on conses."
  (let ((*el-reader//read-objects* nil))
    (let ((o (make-hash-table)))
      (setf (gethash :x o) :y)
      (should (eq o (el-reader//replace-placeholders o)))
      (elr-test/with-objects
       (should (eq o (el-reader//replace-placeholders o)))))))

(ert-deftest elr/internal/replace-placeholders-comprehensive ()
  "Try to be thorough with this one."
  (let ((ps (list (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)))
        (i 0))
    (let ((h (el-reader//ht (seq-elt ps 0) (seq-elt ps 1))))
      (push (vector 1 (seq-elt ps 0) (seq-elt ps 2)) *el-reader//read-objects*)
      (push (vector 2 (seq-elt ps 1) :value) *el-reader//read-objects*)
      (push (vector 3 (seq-elt ps 2) :some) *el-reader//read-objects*)

      (el-reader//replace-placeholders h)
      
      (should (= (length (hash-table-keys h)) 1))
      (should (eq (gethash :some h) :value)))))

(ert-deftest elr-internal/test-num->placeholder ()
  "Test `el-reader//num->placeholder'."
  (let ((triples (list (vector 1 (cons nil nil) nil)
                       (vector 2 (cons nil nil) nil)
                       (vector 3 (cons nil nil) nil)
                       (vector 4 (cons nil nil) nil)
                       (vector 5 (cons nil nil) nil))))
    (seq-do
     (lambda (trpl)
       (push trpl *el-reader//read-objects*))
     triples)
    (should (equal
             (seq-map
              (lambda (trpl)
                (eq (el-reader//num->placeholder (seq-elt trpl 0))
                    (seq-elt trpl 1)))
              triples)
             '(t t t t t)))))

(ert-deftest elr-internal/test-deep-follow ()
  "Test `el-reader//deep-follow-replacement'."
  (let ((ps (list (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)
                  (cons nil nil)))
        (i 0))
    (let ((h (el-reader//ht (seq-elt ps 0) (seq-elt ps 1))))
      (push (vector 1 (seq-elt ps 0) (seq-elt ps 2)) *el-reader//read-objects*)
      (push (vector 2 (seq-elt ps 1) :value) *el-reader//read-objects*)
      (push (vector 3 (seq-elt ps 2) :some) *el-reader//read-objects*)

      (should (eq (el-reader//deep-follow-replacement (seq-elt ps 0)) :some)))))

(ert-deftest elr-internal/test-get-placeholder-replacement ()
  "Sets, then recieves a placeholder object."
  (let ((*el-reader//read-objects* nil)
        (placeholder (cons nil nil)))
    (push (vector 1 placeholder nil) *el-reader//read-objects*)
    (should (null (el-reader//get-placeholder-replacement placeholder)))
    (el-reader//set-placeholder placeholder :some-value)
    (should
     (eq :some-value (el-reader//get-placeholder-replacement placeholder)))))

(ert-deftest elr-test/invalid-syntax ()
  "Reads invalid syntax, i.e. #<buffer>"
  (should-error (el-reader/read "#<stuff>") :type 'invalid-read-syntax))

(ert-deftest elr-test/read-invalid-dot ()
  "Reads a dot, which is invalid."
  (should-error (el-reader/read ".") :type 'invalid-read-syntax))

;; (ert-deftest elr-test/1.0-pos-inf ()
;;   "Reads the canonical positive infinity."
;;   (should (= (el-reader/read "1.0e+INF")
;;              (read "1.0e+INF"))))

;; (let ((inf ".1e+INF"))
;;   (list (type-of (read inf))
;;         (read inf)))

;; 1.1e+INF

;; (/ 0.0 0.0)

;; -1.0e+NaN

;; (let ((inf "+1.0e+NaN"))
;;   (list (type-of (read inf))
;;         (read inf)))
