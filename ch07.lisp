;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.25
;;by quinton from ANSI Common Lisp chapter 7

(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))
