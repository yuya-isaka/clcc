;;;; cat.lisp : ファイルの連結

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf))

(defpackage :cat
  (:use :cl :uiop))
(in-package :cat)

;; オプションの定義
(defvar *b-flag* nil "空行以外に番号を付ける")
(defvar *n-flag* nil "行に番号を付ける")
(defvar *s-flag* nil "連続した空行を一つにまとめる")

;; ファイルを表示する関数
(defun cat (stream &optional (line-number 1))
  (let ((previous-empty-line nil))
    (loop for line = (read-line stream nil nil)
          while line do
            (if (zerop (length line))
                (progn
                 (unless (and *s-flag* previous-empty-line)
                   (if *n-flag*
                       (format t "~6d  ~%" line-number)
                       (format t "~%")))
                 (incf line-number)
                 (setf previous-empty-line t))
                (progn
                 (if (or *b-flag* *n-flag*)
                     (format t "~6d  ~a~%" line-number line)
                     (format t "~a~%" line))
                 (incf line-number)
                 (setf previous-empty-line nil)))))
  line-number)

;; メイン関数
(defun main ()
  (setf *b-flag* (find "-b" (uiop:command-line-arguments) :test 'string=)
    *n-flag* (find "-n" (uiop:command-line-arguments) :test 'string=)
    *s-flag* (find "-s" (uiop:command-line-arguments) :test 'string=))
  (when (and *b-flag* *n-flag*)
        (setf *n-flag* nil))

  (if (null (uiop:command-line-arguments))
      (cat *standard-input*)
      (dolist (file-name (uiop:command-line-arguments))
        (handler-case
            (with-open-file (stream file-name :direction :input)
              (cat stream))
          (error (e)
            (format *error-output* "Error opening file ~a: ~a~%" file-name e)
            (uiop:quit 1))))))

;; プログラムの開始
(main)
