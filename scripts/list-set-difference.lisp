(defun atom-in-list (atom list)
"Check if atom is in list"
  (cond
    ((null list) nil)
    ((eq atom (car list)) t)
    (t (atom-in-list atom (cdr list)))
  )
)

(defun list-set-difference (list1 list2)
"Constructs list with difference of two sets"
  (cond
    ((null list1) nil)
    ((not (atom-in-list (car list1) list2)) (cons (car list1) (list-set-difference (cdr list1) list2)))
    (t (list-set-difference (cdr list1) list2))
  )
)

(defun check-list-set-difference (name input1 input2 expected)
"Execute `list-set-difference' on `input1 input2', 
compare result with `expected' and print comparison status"
  (format t "Test ~a... ~:[FAILED~;passed~]~%"
    name (equal (list-set-difference input1 input2) expected) 
  )
)

(defun test-list-set-difference ()
  (check-list-set-difference "1" '(1 2 3 4) '(3 4 5 6) '(1 2))
  (check-list-set-difference "2" '(1 2 3 4) '(6 3 5 1) '(2 4))
  (check-list-set-difference "3" '(9 1 5 3 7) '(7 5 3 1) '(9))
  (check-list-set-difference "4" '(1 2 3 4) '(5 6) '(1 2 3 4))
  (check-list-set-difference "5" '(1 2 3 4) '(1 2 3 4) nil)
  (check-list-set-difference "6" '(nil) '(nil) nil)
)

(test-list-set-difference)