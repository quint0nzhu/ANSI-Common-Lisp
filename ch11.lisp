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
