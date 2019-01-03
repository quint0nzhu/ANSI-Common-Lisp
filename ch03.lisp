;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.03
;;by quinton from ANSI Common Lisp chapter 3

(defun n-elts (elt n);;将n个元素展开成一个列表
  (if (> n 1)
      (list n elt)
      elt))

(defun compr (elt n lst);;将n个相邻的元素压缩成一个两个值的列表(1 1 1 1 1)=>(5 1)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun compress (x);;游程压缩算法(0 1 1 1 0 0 0 0 1)=>(0 (3 1) (4 0) 1)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun list-of (n elt);;将两值列表展开成列表(3 1)=>(1 1 1)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(defun uncompress (lst);;游程解压缩算法(0 (3 1) (4 0) 1)=>(0 1 1 1 0 0 0 0 1)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

(defun mirror? (s);;判断是否为回文字符串
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))

(defun nthmost (n lst);;返回一个列表中第n大的元素
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

(defun proper-list? (x);;判断一个列表否为正规列表
  (or (null x)
      (and (consp x)
           (proper-list? (cdr x)))))

