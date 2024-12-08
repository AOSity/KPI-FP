(defun add-next-reducer (&key transform)
"Returns a function suitable for use with 'reduce'"
  (lambda (element result)
    (let* ((current (if transform (funcall transform element) element))
           (next (and result (caar result)))
           (pair (cons current next))
          )
          (cons pair result)
    )
  )
)

(defun check-add-next-reducer (name input expected &optional transform)
"Execute 'reduce' with 'add-next-reducer' on 'input, compare with 'expected' and print status"
  (let ((result (reduce (add-next-reducer :transform transform) input :from-end t :initial-value nil)))
       (format t "Test ~a... ~:[FAILED~;passed~]~%" name (equal result expected))
  )
)


(defun test-add-next-reducer ()
  (check-add-next-reducer "1" '(1 2 3) '((1 . 2) (2 . 3) (3 . nil)))
  (check-add-next-reducer "2" '(1 2 3) '((2 . 3) (3 . 4) (4 . nil)) #'1+)
  (check-add-next-reducer "4" '(4) '((4 . nil)))
  (check-add-next-reducer "3" '() '())
  (check-add-next-reducer "5" '(10 20 30) '((100 . 400) (400 . 900) (900 . nil)) (lambda (x) (* x x)))
)

(test-add-next-reducer)
