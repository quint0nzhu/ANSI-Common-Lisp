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

(defun leap-year ();;判断系统年份是否为闰年，本例假定总是闰年
  t)

(defun month-length (mon);;返回一个月的天数
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-year) 29 28))
    (otherwise "unknown month")))

