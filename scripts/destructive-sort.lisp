(defun destructive-sort (lst)
"Constructs sorted list by non-descending,
 using insertion sort, linear search from the right"
  (let ((sorted-lst (copy-list lst)))
    (loop for i from 1 below (list-length sorted-lst) do
      (let ((x (nth i sorted-lst)) (j i))
        (loop while (and (> j 0) (< x (nth (- j 1) sorted-lst))) do
          (setf (nth j sorted-lst) (nth (- j 1) sorted-lst))
          (decf j)
        )
        (setf (nth j sorted-lst) x)
      )
    )
    sorted-lst
  )
)

(defun check-destructive-sort (name input expected)
"Execute `destructive-sort' on `input',
 compare result with `expected' and print comparison status"
  (format t "Test ~a... ~:[FAILED~;passed~]~%"
    name (equal (destructive-sort input) expected) 
  )
)

(defun test-destructive-sort ()
  (check-destructive-sort "1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-destructive-sort "2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-destructive-sort "3" '(4 1 3 2 5) '(1 2 3 4 5))
  (check-destructive-sort "4" '(5 5 3 3 1) '(1 3 3 5 5))
  (check-destructive-sort "5" '(1 -1 2 -2 0 0) '(-2 -1 0 0 1 2))
  (check-destructive-sort "6" '() '())
)

(test-destructive-sort)