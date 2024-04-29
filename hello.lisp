(defun fib (n)
  "Return the nth Fibonacci number."
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 10)

(format t "Hello world")

(defun many (n)
  (values n (* n 2) (* n 3)))

(multiple-value-list (many 3))

(nth-value 2 (many 3))

(multiple-value-bind (first second third) (many 3) (list first second third))

(format t (print "Hello"))

(uiop:read-file-string (merge-pathnames #p"example.lisp" #p"home"))

(with-open-file (fd #p"./tmp.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (dotimes (i 100)
    (format fd "~10,d~%" (random 100))))

(format *error-output* "エラー発生、~A~%" "エラーだよ")

(first nil)
(rest nil)

(append `(d a) `(a b))
(cons `(d a) `(a b))
(cons `a `b)
(cons `b nil)

(print most-positive-fixnum)

(print least-positive-double-float)

pi

(defun area (x r)
  (case x
    (`square (* r r))
    (t r)))

(area `square 3)

(member 3 `(1 2 3 4))

(consp `(1))
(listp `(1 2 3))
(eval (atom `(1)))

(eql `(1 2 3) `(1 2 3))

(ash 2 1000)

(with-open-file (stream #p"example.txt" :direction :input)
  (file-position stream 100)
  (let ((data (read-char stream nil)))
    (when data
          (format t "Character at position 100: ~a~%" data))))