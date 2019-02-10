;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.06
;;by quinton from ANSI Common Lisp chapter 11

(defstruct rect;;定义一个矩形结构体
  height width)

(defstruct circ;;定义一个圆形结构体
  radius)

(defun area-struct (x);;判断参数的类型，再计算矩形或圆形面积的函数
  (cond ((rect-p x)
         (* (rect-height x) (rect-width x)))
        ((circ-p x)
         (* pi (expt (circ-radius x) 2)))))

(defclass rectangle ();;定义一个矩形类
  (height width))

(defclass circle ();;定义一个圆形类，访问槽的方法和初始化槽的参数名及生成对象时槽的默认值都给出了
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)
   (center :accessor circle-center
           :initarg :center
           :initform (cons 0 0))))

(defmethod area ((x rectangle));;求矩形面积的方法，参数直接要求为rectangle类型
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle));;求圆形面积的方法，参数直接要求为circle类型
  (* pi (expt (slot-value x 'radius) 2)))

(defclass colored ();;颜色类
  (color))

(defclass colored-circle (circle colored);;有颜色的圆形类
  ())

(defclass tabloid ();;类变理的测试类，所有对象共享一个槽
  ((top-story :accessor tabloid-story
              :allocation :class)))

(defclass graphic ();;定义一个图形类包括颜色和是否可见两个槽
  ((color :accessor graphic-color
          :initarg :color)
   (visible :accessor graphic-visible
            :initarg :visible
            :initform t)))

(defclass screen-circle (circle graphic);;多重继承了两个类，并获得了两个基类的槽，可以修改基类槽的属性
  ((color :initform 'purple)))

(defclass sculpture () (height width depth));;复杂的类定义层级

(defclass statue (sculpture) (subject));;复杂的类定义层级

(defclass metalwork () (metal-type));;复杂的类定义层级

(defclass casting (metalwork) ());;复杂的类定义层级

(defclass cast-statue (statue casting) ());;复杂的类定义层级

(defmethod combine (x y);;通用函数，用来把两个参数组成一个列表
  (list x y))

(defclass stuff ();;只有一个名字的某样东西，作为基类
  ((name :accessor name
         :initarg :name)))

(defclass ice-cream (stuff);;stuff的子类
  ())

(defclass topping (stuff);;stuff的子类
  ())

(defmethod combine ((ic ice-cream) (top topping));;两个参数均特化为某个特定类型
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          (name top)))

(defmethod combine ((ic ice-cream) x);;第一个参数被特化的版本，当第二个参数不是topping的实例时，会调用该方法
  (format nil "~A ice-cream with ~A."
          (name ic)
          x))

(defmethod combine ((x number) (y number));;两个参数均为数值类型时的特化版本
  (+ x y))

(defmethod combine ((x (eql 'powder)) (y (eql 'spark)));;对单一的对象做特化，注意是对象，不是类型，用eql来决定对象是否相等，这种特化优先级最高
  'boom)

(defmethod combine ((x (eql 'powder)) (y (eql 'spark)));;此方法特化情况与上面的方法是一样的，所以覆盖了上一个方法
  'kaboom)

(defclass speaker ();;定义一个空类别
  ())

(defmethod speak ((s speaker) string);;speaker的一个主要的方法
  (format t "~A" string))

(defclass intellectual (speaker);;有智慧的speaker
  ())

(defmethod speak :before ((i intellectual) string);;调用主方法speak之前，先调用这个方法
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string);;调用主方法speak之后，再调用这个方法
  (princ " in some sense"))

(defmethod speak :before ((s speaker) string);;为基类写的:before方法
  (princ "I think "))

(defclass courtier (speaker);;另一个从speaker继承的类别，用来验证:around方法
  ())

(defmethod speak :around ((c courtier) string);;先调用这个方法，再调用其他方法
  (format t "Does the King believe that ~A?" string)
  (if (eql (read) 'yes)
      (if (next-method-p)
          (call-next-method))
      (format t "Indeed, it is a preposterous idea. ~%"))
  'bow)

(defgeneric price (x);;使+方法组合
  (:method-combination +))

(defclass jacket ();;夹克类别
  ())

(defclass trousers ();;裤子类别
  ())

(defclass suit (jacket trousers);;正装类别，继承自夹克和裤子类别
  ())

(defmethod price + ((jk jacket));;待组合的方法，取得夹克的价格
  350)

(defmethod price + ((tr trousers));;待组合的方法，取得裤子的价格
  200)

(defpackage "CTR";;定义CTR包，只输出COUNTER，INCREMENT，CLEAR三个方法
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))

(in-package ctr);;使用CTR包

(defclass counter ();;定义一个计数器类别
  ((state :initform 0)))

(defmethod increment ((c counter));;计数器加一方法
  (incf (slot-value c 'state)))

(defmethod clear ((c counter));;计数器归零方法
  (setf (slot-value c 'state) 0))
