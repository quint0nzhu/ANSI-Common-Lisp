;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.31
;;by quinton from ANSI Common Lisp chapter 8

;;(defpackage "MY-APPLICATION";;定义一个MY-APPLICATION包，引用了COMMON-LISP包和MY-UTILITIES包，别名叫APP，只导出WIN、LOSE、DRAW三个符号
;;  (:use "COMMON-LISP" "MY-UTILITIES")
;;  (:nicknames "APP")
;;  (:export "WIN" "LOSE" "DRAW"))

;;(in-package my-application)

(defun noise (animal);;关键字符号，在哪个包里都可见
  (case animal
    (:dog :woof)
    (:cat :meow)
    (:pig :oink)))

(defparameter *words* (make-hash-table :size 10000));;哈希表，用来保存出现在某个单词后面的单词及其出现次数

(defconstant maxword 100);;定义最长单词的长度

(defun punc (c);;根据字符返回一个符号
  (case c
    (#\. '|.|)
    (#\, '|,|)
    (#\; '|;|)
    (#\! '|!|)
    (#\? '|?|)))

(let ((prev `|.|));;将符号插入哈希表
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
    (setf prev symb)))

(defun read-text (pathname);;读取一个文本文件，依次记录里面出现的单词（作为哈希表的键）及其出现在其后的单词及出现的频数
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof)
              (read-char s nil :eof)))
          ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
            (progn
              (setf (aref buffer pos) c)
              (incf pos))
            (progn
              (unless (zerop pos)
                (see (intern (string-downcase
                              (subseq buffer 0 pos))))
                (setf pos 0))
              (let ((p (punc c)))
                (if p (see p)))))))))

(defun random-next (prev);;随机选择某个单词后面出现的一个单词
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))

(defun generate-text (n &optional (prev '|.|));;根据随机选择的单词，生成一篇文章
  (if (zerop n)
      (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))
