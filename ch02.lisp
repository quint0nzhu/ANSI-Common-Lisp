;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.02
;;by quinton from ANSI Common Lisp chapter 2

(defun askem (string);;提示使用者输入，并返回任何输入的东西
  (format t "~A" string)
  (read))

(defun ask-number ();;输入的为数字就会输出，否则一直提示重新输入
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defun show-squares (start end);;打印从start到end为止的数的平方的迭代算法
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-r (i end);;打印从start到end为止的数的平方的递归算法
  (if (> i end)
      'done
      (progn
        (format t "~A ~A~%" i (* i i))
        (show-squares (+ i 1) end))))
