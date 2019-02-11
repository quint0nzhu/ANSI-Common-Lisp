;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.10
;;by quinton from ANSI Common Lisp chapter 12

(defun our-tailp (x y);;判断x是否为y的尾部
  (or (eql x y)
      (and (consp y)
           (our-tailp x (cdr y)))))

(defun our-copy-list (lst);;返回一个不与原始列表共享顶层列表结构的新列表
  (if (null lst)
      nil
      (cons (car lst) (our-copy-list (cdr lst)))))

(defun our-copy-tree (tr);;返回一个连原始列表的树型结构也不共享的新列表
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(defun make-queue ();;创建一个空队列
  (cons nil nil))

(defun enqueue (obj q);;入队列操作
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q);;出队列操作
  (pop (car q)))

(defun nconc2 (x y);;合并两个列表
  (if (consp x)
      (progn
        (setf (cdr (last x)) y)
        x)
      y))

(defun our-mapcan (fn &rest lsts);;将函数的返回值用nconc拼接在一起
  (apply #'nconc (apply #'mapcar fn lsts)))

(defun mappend (fn &rest lsts);;上面那个函数的无损版本
  (apply #'append (apply #'mapcar fn lsts)))

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

(defun bsti (obj bst <);;向BST中破坏性的插入一个节点的具体实现
  (let ((elt (node-elt bst)))
    (if (eql obj elt)
        bst
        (if (funcall < obj elt)
            (let ((l (node-l bst)))
              (if l
                  (bsti obj l <)
                  (setf (node-l bst)
                        (make-node :elt obj))))
            (let ((r (node-r bst)))
              (if r
                  (bsti obj r <)
                  (setf (node-r bst)
                        (make-node :elt obj))))))))

(defun bst-insert! (obj bst <);;向BST插入一个节点，具有破坏性
  (if (null bst)
      (make-node :elt obj)
      (progn (bsti obj bst <)
             bst)))

(defun set-par (par dir val);;为par设置左子树的值或右子树的值
  (case dir
    (:l (setf (node-l par) val))
    (:r (setf (node-r par) val))))

(defun cutmax (bst par dir);;删除BST最大节点，破坏性的
  (if (node-r bst)
      (cutmax (node-r bst) bst :r)
      (progn
        (set-par par dir (node-l bst))
        (node-elt bst))))

(defun cutmin (bst par dir);;删除BST最小节点，破坏性的
  (if (node-l bst)
      (cutmin (node-l bst) bst :l)
      (progn
        (set-par par dir (node-r bst))
        (node-elt bst))))

(defun replace-node (old new);;把新节点赋给旧节点
  (setf (node-elt old) (node-elt new)
        (node-l old) (node-l new)
        (node-r old) (node-r new)))

(defun cutprev (bst root prev);;找到左子树中最大的节点，prev为它的父节点
  (if (node-r bst)
      (cutprev (node-r bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-r prev) (node-l bst))
            root)
          (progn
            (setf (node-r bst) (node-r root))
            bst))))

(defun cutnext (bst root prev);;找到右子树中最小的节点，prev为它的父节点
  (if (node-l bst)
      (cutnext (node-l bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-l prev) (node-r bst))
            root)
          (progn
            (setf (node-l bst) (node-l root))
            bst))))

(defun del-root (bst);;破坏性的删除一棵BST的根节点
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2))
                 (cutnext r bst nil)
                 (cutprev l bst nil))))))

(defun bst-delete (obj bst <);;破坏性的删除BST的一个节点
  (if (null bst)
      nil
      (if (eql obj (node-elt bst))
          (del-root bst)
          (progn
            (if (funcall < obj (node-elt bst))
                (setf (node-l bst) (bst-delete obj (node-l bst) <))
                (setf (node-r bst) (bst-delete obj (node-r bst) <)))
            bst))))
