;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.02
;;by quinton from ANSI Common Lisp chapter 9

(defun palindrome? (x);;回文字符串判断
  (let ((mid (/ (length x) 2)))
    (equal (subseq x 0 (floor mid))
           (reverse (subseq x (ceiling mid))))))

(defun our-truncate (n);;截断函数
  (if (> n 0)
      (floor n)
      (ceiling n)))

(defun sq (x) (* x x));;返回参数的平方

(defun mag (x y z);;返回点到原点的距离
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z);;由非单位向量变成单位向量
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil));;定义一个三维空间中的点
  x y z)

(defun distance (p1 p2);;定义两个点之间的距离
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c);;返回一元二次方程较小的根
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(defstruct surface color);;定义一个表面结构体

(defstruct (sphere (:include surface));;定义一个球结构体
  radius center)

(defparameter *world* nil);;定义一个场景世界

(defconstant eye (make-point :x 0 :y 0 :z 200));;定义观测点的位置

(defun defsphere (x y z r c);;构造一个球，并放入场景世界
  (let ((s (make-sphere
            :radius r
            :center (make-point :x x :y y :z z)
            :color c)))
    (push s *world*)
    s))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x (+ (x pt) (* n xr))
                    :y (+ (y pt) (* n yr))
                    :z (+ (z pt) (* n zr))))))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))



