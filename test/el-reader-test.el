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
  ())

(el-reader/read "1.0e+INF")

1.1e+INF

(/ 0.0 0.0)
