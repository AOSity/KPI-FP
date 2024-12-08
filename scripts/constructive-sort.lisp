(defun insert (x lst test)
"Insert a (key . value) pair 'x' into 'lst'"
  (cond ((null lst) (list x))
        ((funcall test (car (car lst)) (car x)) (cons (car lst) (insert x (cdr lst) test)))
        (t(cons x lst))
  )
)

(defun constructive-sort (lst &key (key #'identity) (test #'>=))
"Sort the list 'lst' according to 'test' and 'key'"
  (let* ((key-lst (mapcar (lambda (x) (cons (funcall key x) x)) lst))
         (key-sorted (reduce (lambda (sorted x) (insert x sorted test)) key-lst :initial-value '()))
        )
        (mapcar #'cdr (reverse key-sorted))
  )
)

(defun check-constructive-sort (name input expected &key (key #'identity) (test #'>=))
"Execute `constructive-sort' on `input', compare result with `expected' and print status"
  (format t "Test ~a... ~:[FAILED~;passed~]~%" 
   name (equal (constructive-sort input :key key :test test) expected)
  )
)

(defun test-constructive-sort ()
  (check-constructive-sort "1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-constructive-sort "2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-constructive-sort "3" '(4 1 3 2 5) '(1 2 3 4 5))
  (check-constructive-sort "4" '(5 5 3 3 1) '(1 3 3 5 5))
  (check-constructive-sort "5" '(1 -1 2 -2 0 0) '(-2 -1 0 0 1 2))
  (check-constructive-sort "6" '() '())
  (check-constructive-sort "7" '(1 3 2 5 4) '(5 4 3 2 1) :test #'<=)
  (check-constructive-sort "8" '((a 3) (b 1) (c 2)) '((b 1) (c 2) (a 3)) :key #'cadr)
  (check-constructive-sort "9" '((a 3) (b 1) (c 2)) '((a 3) (c 2) (b 1)) :key #'cadr :test #'<=)
)

(test-constructive-sort)
