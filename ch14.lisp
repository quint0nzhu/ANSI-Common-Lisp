;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.13
;;by quinton from ANSI Common Lisp chapter 14

(setf *print-circle* t);;设置后可打印出环状结构的对象

(deftype proseq ();;定义一个复合类型标识符
  '(or vector (and list (not (satisfies circular?)))))

;;(deftype multiple-of (n);;定义一个n的倍数的标识符
;;  `(and integer (satisfies (lambda (x)
;;                             (zerop (mod x ,n))))))

(deftype multiple-of (n);;定义一个n的倍数的标识符
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
          #'(lambda (x) (zerop (mod x n))))
          `(and integer (satisfies ,predicate))))

(defun copy-file (from to);;拷贝一个二进制文件
  (with-open-file (in from :direction :input
                           :element-type 'unsigned-byte)
    (with-open-file (out to :direction :output
                            :element-type 'unsigned-byte)
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (declare (fixnum i))
        (write-byte i out)))))

(set-macro-character #\';;“’”读取宏的实现
                     #'(lambda (stream char)
                         (list (quote quote) (read stream t nil t))))

(set-dispatch-macro-character #\# #\?;;定义#？作为返回一个整数列表的读取宏
                              #'(lambda (stream char1 char2)
                                  (list 'quote
                                        (let ((lst nil))
                                          (dotimes (i (+ (read stream t nil t) 1))
                                            (push i lst))
                                          (nreverse lst)))))

(set-macro-character #\} (get-macro-character #\)));;定义#{x y}作为返回介于x与y之间的整数列表，包含x与y
(set-dispatch-macro-character #\# #\{
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\} stream t)))
                                    (do ((i (car pair) (+ i 1)))
                                        ((> i (cadr pair))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))

(defun even/odd (ns);;接受一个数字的列表并返回偶数与奇数列表
  (loop for n in ns
        if (evenp n)
          collect n into evens
        else collect n into odds
        finally (return (values evens odds))))

(defun sum (n);;求1到n的累加和
  (loop for x from 1 to n
        sum x))

(defun user-input (prompt);;等待用户输入一个表达式，在输入有语法错误时不会中断执行
  (format t prompt)
  (let ((str (read-line)))
    (or (ignore-errors (read-from-string str))
        nil)))
