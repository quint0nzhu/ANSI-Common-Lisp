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

(defun sphere-intersect (s pt xr yr zr);;返回光线与球面的照射点
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

(defun intersect (s pt xr yr zr);;根据不同平面，调用不同函数，这里只有球面
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-normal (s pt);;返回球面的法向量
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun normal (s pt);;根据不同平面，调用不同函数，这时只有球面
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun lambert (s int xr yr zr);;朗伯定律，返回平面上一点被光照后，反射光的强度
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defun first-hit (pt xr yr zr);;返回光线最先照射到的平面和照射点
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun sendray (pt xr yr zr);;返回一个数值介于0至1，表示亮度，光线没有击中任何东西，返回0
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun color-at (x y);;生成在坐标x，y处的颜色数据，缩放成0至255之间的整数
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun tracer (pathname &optional (res 1));;打开要生成的pgm文件，并写入颜色数据
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

(defun ray-test (&optional (res 1));;光线追踪算法测试，先在中间偏上位置生成三个球体，加入场景世界，然后又在偏下位置生成5X6个球体加入场景世界，最后运用光线追踪算法显示这些球体，并将显示生成的图片保存成pgm格式的文件
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))
