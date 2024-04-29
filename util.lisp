;; xs > ysかどうか
(defun longer-than (xs ys)
  (> (length xs) (length ys)))

;; 要素を検索して取り出し
(defun find-element (x ys)
  (caar (member x ys)))

;; フィボナッチ
(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; FizzBuzz
(defun fizzbuzz-dotimes (x)
  (dotimes (n x)
    (cond
     ((= 0 (mod (1+ n) 15)) (format t "FizzBuzz "))
     ((= 0 (mod (1+ n) 3)) (format t "Fizz "))
     ((= 0 (mod (1+ n) 5)) (format t "Buzz "))
     (t (format t "~d " (1+ n))))))

;; 階乗
(defun fact (x)
  (let ((result 1))
    (dotimes (n x result)
      (setf result (* result (1+ n))))))

;; 素数判定
(defun fast-primep (n ps)
  (dolist (m ps t)
    (cond
     ((zerop (mod n m)) (return))
     ((< n (* m m)) (return t)))))
(defun fast-prime (n)
  (do ((ps `(2)) (m 3 (+ m 2)))
      ((< n m) ps)
    (if (fast-primep m ps)
        (setf ps (append ps (list m))))))

;; 乗算
(defun product (s e)
  (let ((result 1))
    (if (> s e) result (product-rec (1+ s) e (* result s)))))
(defun product-rec (s e result)
  (if (> s e) result (product-rec (1+ s) e (* result s))))

;; スタック
(defvar *stack*)
(defun push-stack (data) (setf *stack* (cons data *stack*)))
(defun pop-stack () (prog1 (car *stack*) (setf *stack* (cdr *stack*))))
; (setf *stack* nil)
; (push-stack 10)
; (push-stack 30)
; (pop-stack)
; (print *stack*)

;; 要素が一つのリスト？
(defun singlep (xs)
  (and (consp xs) (null (cdr xs))))
; (singlep `(3))