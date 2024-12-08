(defun insert (x lst)
"Insert 'x' into 'lst' before first smaller element"
  (cond ((null lst) (list x))
        ((>= x (car lst)) (cons x lst))
        (t (cons (car lst) (insert x (cdr lst))))
  )
)

(defun constructive-sort-helper (remaining sorted)
"Inserts values from 'remaining' into 'sorted' in descending order
 and returns reversed 'sorted' when all elements of 'remaining' used"
  (if (null remaining)
      (reverse sorted)
      (constructive-sort-helper (cdr remaining) (insert (car remaining) sorted))
  )
)

(defun constructive-sort (lst)
"Constructs sorted list by non-descending,
 using insertion sort, linear search from the right"
  (constructive-sort-helper lst '())
)

(defun check-constructive-sort (name input expected)
"Execute `constructive-sort' on `input',
 compare result with `expected' and print comparison status"
  (format t "Test ~a... ~:[FAILED~;passed~]~%"
    name (equal (constructive-sort input) expected) 
  )
)

(defun test-constructive-sort ()
  (check-constructive-sort "1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-constructive-sort "2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-constructive-sort "3" '(4 1 3 2 5) '(1 2 3 4 5))
  (check-constructive-sort "4" '(5 5 3 3 1) '(1 3 3 5 5))
  (check-constructive-sort "5" '(1 -1 2 -2 0 0) '(-2 -1 0 0 1 2))
  (check-constructive-sort "6" '() '())
)

(test-constructive-sort)