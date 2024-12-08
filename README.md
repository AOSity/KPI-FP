<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент:</b> <i>Горбуль Андрій Олександрович КВ-11</i><p>
<p align="right"><b>Рік:</b> <i>2024</i><p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де це
доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: `key` та `test` , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями.
При цьому `key` має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом. Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини 4
Алгоритм сортування вставкою №2 (з лінійним пошуком справа) за незменшенням.
<p align="center">
<img src="insertion-sort-2.png">
</p>

## Лістинг реалізації першої частини завдання
```lisp
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
```

### Тестові набори та утиліти першої частини
```lisp
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
```

### Тестування першої частини
```bash
$ sbcl --script  scripts/constructive-sort.lisp 
Test 1... passed
Test 2... passed
Test 3... passed
Test 4... passed
Test 5... passed
Test 6... passed
Test 7... passed
Test 8... passed
Test 9... passed
```
## Варіант другої частини 4
Написати функцію `add-next-reducer` , яка має один ключовий параметр — функцію `transform`.
`add-next-reducer` має повернути функцію, яка при застосуванні в якості першого аргументу 
`reduce` робить наступне: кожен елемент списку-аргументу `reduce` перетворюється на точкову 
пару, де в комірці CAR знаходиться значення поточного елемента, а в комірці CDR знаходиться 
значення наступного елемента списку (тобто того, що знаходиться "справа"). Якщо функція 
`transform` передана, тоді значення поточного і наступного елементів, що потраплять у 
результат, мають бути змінені згідно `transform` . Обмеження, які накладаються на 
використання функції-результату `add-next-reducer` при передачі у `reduce` визначаються 
розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових 
параметрів функції `reduce` `from-end` та `initial-value` ). `transform` має виконатись мінімальну 
кількість разів.
```lisp
CL-USER> (reduce (add-next-reducer)
                 '(1 2 3)
                 :from-end ...
                 :initial-value ...)
((1 . 2) (2 . 3) (3 . nil))

CL-USER> (reduce (add-next-reducer :transform #'1+)
                 '(1 2 3)
                 :from-end ...
                 :initial-value ...)
((2 . 3) (3 . 4) (4 . nil))
```

## Лістинг реалізації другої частини завдання
```lisp
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
```

### Тестові набори та утиліти другої частини
```lisp
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
```

### Тестування другої частини
```bash
$ sbcl --script  scripts/add-next-reducer.lisp 
Test 1... passed
Test 2... passed
Test 4... passed
Test 3... passed
Test 5... passed
```