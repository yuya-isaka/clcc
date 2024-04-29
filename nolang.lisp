(defclass instruction ()
    ((code :accessor code :initarg :code)
     (opts :accessor opts :initarg :opts)))

(defmethod inspecttt ((insn instruction))
  (format nil "~a <~{~a~^, ~}>" (code insn) (opts insn)))

(defvar *label-id* 0)

(defclass label ()
    ((label :accessor label :initarg :label)
     (pos :accessor pos :initarg :pos :initform -1)
     (id :accessor id :initform (incf *label-id*))))

(defmethod inspecttt ((lbl label))
  (format nil "~a <~a@~a>" (label lbl) (id lbl) (pos lbl)))

(defclass evaluator ()
    ((stack :accessor stack :initform nil)
     (pc :accessor pc :initform 0)))

(defun evaluate (sequence evaluator)
  (loop for insn = (nth (pc evaluator) sequence) while insn do
          (dispatch insn evaluator))
  (first (stack evaluator)))

(defun dispatch (insn evaluator)
  (let ((code (code insn)))
    (case code
      (:nop nil)
      (:push (push (first (opts insn)) (stack evaluator)))
      (:pop (pop (stack evaluator)))
      (:dup (let ((item (pop (stack evaluator))))
              (push item (stack evaluator))
              (push item (stack evaluator))))
      (:add (push (+ (pop (stack evaluator)) (pop (stack evaluator))) (stack evaluator)))
      (:sub (push (- (pop (stack evaluator)) (pop (stack evaluator))) (stack evaluator)))
      (:mul (push (* (pop (stack evaluator)) (pop (stack evaluator))) (stack evaluator)))
      (:div (push (/ (pop (stack evaluator)) (pop (stack evaluator))) (stack evaluator)))
      (:not (push (not (pop (stack evaluator))) (stack evaluator)))
      (:smaller (push (< (pop (stack evaluator)) (pop (stack evaluator))) (stack evaluator)))
      (:bigger (push (> (pop (stack evaluator)) (pop (stack evaluator))) (stack evaluator)))
      (:goto (setf (pc evaluator) (pos (first (opts insn)))))
      (:if (when (pop (stack evaluator))
                 (setf (pc evaluator) (pos (first (opts insn))))))
      (t (error "Unknown Opcode: ~A" code)))))

; (defun parse (program)
;   (let ((pc 0)
;         (labels (make-hash-table :test 'equal))
;         (instructions nil))
;     (loop for line in (split-sequence:split-sequence #\Newline program)
;           do (let ((line (string-trim " \t\n\r" line)))
;                (cond
;                 ((string-match "^:\\([a-z]+\\)$" line)
;                   (let ((label (match-string 1 line)))
;                     (unless (gethash label labels)
;                       (setf (gethash label labels) (make-instance 'label :label label)))
;                     (push (gethash label labels) instructions)))

;                 ((string-match "^[a-z]+" line)
;                   (multiple-value-bind (opcode rest) (split-sequence:split-sequence #\Space line :max-splits 1)
;                     (push (make-instance 'instruction :code (intern (string-upcase opcode) :keyword) :opts (list rest)) instructions)))

;                 ((string-match "^\\d+" line)
;                   (push (parse-integer line) instructions))

;                 ((or (string-match "^\\s*" line) (string-match "^#.*" line))
;                   ()))
;                (when (and (consp (car instructions)) (not (label-p (car instructions))))
;                      (setf (pos (car instructions)) pc)
;                      (incf pc))))
;     (remove-if #'label-p (nreverse instructions))))

(defun parse (program)
  (let ((pc 0)
        (labels (make-hash-table :test 'equal))
        (instructions nil))
    (loop for line in (cl-ppcre:split #\newline program)
          do (let ((line (string-trim " \t\n\r" line)))
               (cond
                ((cl-ppcre:scan "^:\\([a-z]+\\)$" line)
                  (let ((label (cl-ppcre:regex-replace-all "^:\\([a-z]+\\)$" line "\\1")))
                    (unless (gethash label labels)
                      (setf (gethash label labels) (make-instance 'label :label label)))
                    (push (gethash label labels) instructions)))

                ((cl-ppcre:scan "^[a-z]+" line)
                  (multiple-value-bind (opcode rest) (cl-ppcre:split #\Space line)
                    ;; debug
                    (format *error-output* "~a~%" opcode)
                    (push (make-instance 'instruction :code (intern (string-upcase (car opcode)) :keyword) :opts (list rest)) instructions)))

                ((cl-ppcre:scan "^\\d+" line)
                  (push (parse-integer line) instructions))

                ((or (cl-ppcre:scan "^\\s*" line) (cl-ppcre:scan "^#.*" line))
                  ()))
               (when (and (consp (car instructions)) (not (label-p (car instructions))))
                     (setf (pos (car instructions)) pc)
                     (incf pc))))
    (remove-if #'label-p (nreverse instructions))))


(defun label-p (item)
  (typep item 'label))

(let ((program "
push 1
:label
push 1
add
dup
push 100000
bigger
if :label
"))
  (let ((parsed-program (parse program)))
    (format t "~%Parsed Program:~%")
    (dolist (insn parsed-program)
      (format t "~A~%" (inspect insn)))
    (format t "~%Evaluation Result: ~A~%" (evaluate parsed-program (make-instance 'evaluator)))))
