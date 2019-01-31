;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.25
;;by quinton from ANSI Common Lisp chapter 7

(defun pseudo-cat (file);;循环读出一个文件中的每一行
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

(defstruct buf;;定义一个环形缓冲区数据结构，包含5个字段
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n);;接受一个缓冲区和一个索引，返回索引所在位置的元素
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n);;更新指定索引处的元素的值
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len);;产生一个新环形缓冲区，长度为len
  (make-buf :vec (make-array len)))

(defun buf-insert (x b);;向环形缓冲区插入一个元素，插入到最后一个元素的后面
  (setf (bref b (incf (buf-end b)))
        x))

(defun buf-pop (b);;返回一个环形缓冲区的第一个元素，接着将start递增
  (prog1
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new b) (buf-end b))))

(defun buf-next (b);;从环形缓冲区读取一个元素，但并不取出
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b);;重置used和new的值为初始值，分别是start和end
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

(defun buf-clear (b);;清空环形缓冲区
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new b) -1 (buf-end b) -1))

(defun buf-flush (b str);;通过将所有作用的元素，写至由第二个参数所给入的流
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(defun stream-subst (old new in out);;一次从输入流读一个字符，直到输入字符匹配要寻找的字符串时，直接写入输出流
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst (old new file1 file2);;old为查询字符串，new为替换字符串，file1为输入文件，file2为输出文件
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst old new in out))))
