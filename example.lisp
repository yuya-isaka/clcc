(require 'uiop)

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if (/= (length args) 1)
        (progn
          (format *error-output* "~A: invalid number of arguments~%" (car (uiop:command-line-arguments)))
          (return-from main 1)))
    (format t "  .globl main~%")
    (format t "main:~%")
    (format t "  mov $~D, %%rax~%" (parse-integer (first args)))
    (format t "  ret~%")
    0))

(main)

