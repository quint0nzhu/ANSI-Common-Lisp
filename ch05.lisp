;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.07
;;by quinton from ANSI Common Lisp chapter 5

(defun read-integer (str);;输入数字字符串，转换成数值，注意return-from语句
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
        (if i
            (setf accum (+ (* accum 10) i))
            (return-from read-integer nil))))
    accum))
