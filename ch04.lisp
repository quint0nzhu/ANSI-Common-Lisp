;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.04
;;by quinton from ANSI Common Lisp chapter 4

(defun finder (obj vec start end);;寻找向量vec内obj是否介于start及end之间
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
        (if (eql obj (aref vec start))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end)
                    obj)))))))

(defun bin-search (obj vec);;二叉搜索算法，设置初始范围及发送控制信号给finder
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun mirror-elt? (s);;判断回文序列使用elt版
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
             ((or (> forward back)
                  (not (eql (elt s forward)
                            (elt s back))))
              (> forward back))))))

(defun second-word (str);;返回一个句子中的第二个单词
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))

(defun constituent (c);;是图形字符且不是空白字符（空白字符原本是图形字符）
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens (str test start);;用来从字符串中取出语元(token)，满足test条件
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defconstant month-names;;月份简写字符串常量数组
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str);;解析月份，返回在数组的索引，并加上1等于月份数字
  (let ((p (position str month-names
                     :test #'string-equal)));;不区分大小写的比较
    (if p
        (+ p 1)
        nil)))

(defun parse-date (str);;解析日期，日月年
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

(defun read-integer (str);;自定义的由字符转成整数的函数
  (if (every #'digit-char-p str)
      (let ((accum 0))
        (dotimes (pos (length str));;pos从0到字符串的长度，每次增1
          (setf accum (+ (* accum 10)
                         (digit-char-p (char str pos)))));;digit-char-p测试并返回对应的整数
        accum)
      nil))

(defstruct (node (:print-function;;二叉搜索树节点类型定义，elt为元素对象，l和r为左右子节点
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))));;打印节点元素
  elt (l nil) (r nil))

(defun bst-insert (obj bst <);;二叉搜索树生成算法，如果是空树，则用obj生成根节点；如果根节点元素值和插入元素相同，则返回原树根结点；如果插入元素小于根节点元素则生成并返回一个节点，其中：元素为根节点元素值，左子节点为将obj插入左子树后生成的新子树的根节点，右子节点为原右子树根节点。如果插入元素大于根节点元素则生成并返回一个节点，其中：元素为根节点元素值，右子节点为将obj插入右子树后生成的新子树的根节点，左子节点为原左子树根节点。
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-insert obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-insert obj (node-r bst) <)
                 :l (node-l bst)))))))

(defun bst-find (obj bst <);;二叉搜索树查找算法
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun bst-min (bst);;返回二叉搜索树中的最小节点
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst);;返回二叉搜索树中的最大节点
  (and bst
       (or (bst-max (node-r bst)) bst)))

(defun bst-remove-min (bst);;删除二叉搜索树中的最小节点
  (if (null (node-l bst))
      (node-r bst)
      (make-node :elt (node-elt bst)
                 :l (bst-remove-min (node-l bst))
                 :r (node-r bst))))

(defun bst-remove-max (bst);;删除二叉搜索树中的最大节点
  (if (null (node-r bst))
      (node-l bst)
      (make-node :elt (node-elt bst)
                 :l (node-l bst)
                 :r (bst-remove-max (node-r bst)))))

(defun percolate (bst);;二叉搜索树删除算法中当找到要删除节点后，具体删除节点的算法
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2));;如果被删除节点有左右节点，则随机选择一个子节点
                 (make-node :elt (node-elt (bst-max l));;选左子树中最大值作为新根节点
                            :r r
                            :l (bst-remove-max l))
                 (make-node :elt (node-elt (bst-min r));;选右子树中最小值作为新根节点
                            :r (bst-remove-min r)
                            :l l))))))

(defun bst-remove (obj bst <);;二叉搜索树的删除节点算法
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-remove obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-remove obj (node-r bst) <)
                 :l (node-l bst)))))))

(defun bst-traverse (fn bst);;二叉搜索树的遍历算法
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))
