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

(defun show-squares (start end);;打印数的平方
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun factorial (n);;计算阶乘
  (do ((j n (- j 1))
       (f 1 (* j f)))
      ((= j 0) f)))

(defun sub ();;抛出异常
  (throw 'abort 99))

(defun super ();;捕获异常
  (catch 'abort
    (sub)
    (format t "we'll never see this.")))

(defconstant month;;定义每个月开始的那一天是一年中的第几天
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000);;从2000开始计算

(defun leap? (y);;判断是否为闰年
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun month-num (m y);;根据月份计算该月第一天是全年的第几天
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-days (y);;计算一年有多少天
  (if (leap? y) 366 365))

(defun year-num (y);;计算y年距2000年有多少天
  (let ((d 0))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (incf d (year-days (+ y i)))))))

(defun date->num (d m y);;计算y年m月d日距2000年1月1日有多少天，转换日期至数字
  (+ (- d 1) (month-num m y) (year-num y)))

(defun nmon (n);;根据一年中的第几天转换成几月几日
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (svref month (- m 1)))))))

(defun num-month (n y);;根据年份和天数，分解出月与日
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t        (nmon n)))
      (nmon n)))

(defun num-year (n);;以日期的格式返回年，以及剩余的天数
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num->date (n);;将整数转换回日期
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun date+ (d m y n);;日期加法，日期加上天数得到新的日期
  (num->date (+ (date->num d m y) n)))

