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

(dotimes (x 5 x) (print x))

(defun fact (x)
  (let ((result 1))
    (dotimes (n x result)
      (setf result (* result (1+ n))))))

(fact 3)


(dolist (x `(0 1 2 3)) (print x))


; (time (length (fast-prime 100000)))

(defun rpn (xs)
  (let ((zs nil))
    (dolist (x xs (if (and (consp zs) (null (cdr zs)))
                      (car zs)
                      "invalid expression"))
      (if (numberp x)
          (push x zs)
          (let ((b (pop zs)) (a (pop zs)))
            (if (or (null b) (null a))
                (return "stack underflow"))
            (case
                x
              (+ (push (+ a b) zs))
              (- (push (- a b) zs))
              (* (push (* a b) zs))
              (/ (push (/ a b) zs))
              (t (return "invalid operation"))))))))

(equal (rpn `(1 2 + 3 4 + *)) 21)
(equal (rpn `(1 2 + 3 4 - *)) -3)
(equal (rpn `(1 2 + 3 4 + 5 6 + * *)) 231)
(equal (rpn `(1 2 + 3 4 + 5 6 + * /)) 3/77)
(equal (rpn `(1 2 + 3 4 + * 5 6 + /)) 21/11)

(apply #'+ (mapcar #'length `("abc" "defg" "hijkl" "mnopqr")))

(defun exec-fun (func a b)
  (funcall func a b))

(exec-fun #'+ 1 2)

(mapcar (lambda (x) (* x x)) `(1 2 3))
(funcall #'(lambda (x) (* x x)) 2)

(defun map1 (fn xs)
  (if (null xs)
      nil
      (cons (funcall fn (car xs)) (map1 fn (cdr xs)))))

(defun map2 (fn xs ys)
  (if (or (null xs) (null ys))
      nil)
  (cons (funcall fn (car xs) (car ys)) (map2 fn (cdr xs) (cdr ys))))

(defun filter (pred xs)
  (cond ((null xs) nil)
        ((funcall 'pred (car xs)) (cons (car xs) (filter pred (cdr xs))))
        (t (filter pred (cdr xs)))))

(mapcar (lambda (x) (if (evenp x) x)) '(1 2 3))
(funcall (lambda (x) (if (evenp x) x)) 2)