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

(setf xs (list `a (cons `b nil) (list (cons `c nil)) (list (list (cons `d nil)))))

(setf ys (list (list `a `b `c) (list `d `e `f) (list `g `h `i)))

(setf zs (list (cons `a `b) (cons `c `d) (cons `e `f)))

(print most-positive-fixnum)

(print least-positive-double-float)

(setf a 1 b 2 c 3)

a
b
c

pi

(defun area (x r)
  (case x
    (`square (* r r))
    (t r)))

(area `square 3)

(member 3 `(1 2 3 4))