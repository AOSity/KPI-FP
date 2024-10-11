(defun remove-seconds-and-reverse (list)
"Constructs list with every other element removed and then reversed"
  (cond
    ((null list) nil)
    ((null (third list)) (list (first list)))
    (t (append (remove-seconds-and-reverse (nthcdr 2 list)) (list (first list))))
    ;; or destructive method
    ;; (t (nconc (remove-seconds-and-reverse (nthcdr 2 list)) (list (first list))))
  )
)

(defun check-remove-seconds-and-reverse (name input expected)
"Execute `remove-seconds-and-reverse' on `input', 
compare result with `expected' and print comparison status"
  (format t "Test ~a... ~:[FAILED~;passed~]~%"
    name (equal (remove-seconds-and-reverse input) expected) 
  )
)

(defun test-remove-seconds-and-reverse ()
  (check-remove-seconds-and-reverse "1" '(1 2 a b 3 4 d) '(d 3 a 1))
  (check-remove-seconds-and-reverse "2" '(1 a 2 b 3 c 4 d) '(4 3 2 1))
  (check-remove-seconds-and-reverse "3" '(1) '(1))
  (check-remove-seconds-and-reverse "4" '(1 2) '(1))
  (check-remove-seconds-and-reverse "4" '(1 2 3) '(3 1))
  (check-remove-seconds-and-reverse "5" '('(1 a) 2 b '(3 c) 4 d '(5 e)) '('(5 e) 4 b '(1 a)))
  (check-remove-seconds-and-reverse "6" '() '())
)

(test-remove-seconds-and-reverse)