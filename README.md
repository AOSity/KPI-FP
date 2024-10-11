<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент:</b> <i>Горбуль Андрій Олександрович КВ-11</i><p>
<p align="right"><b>Рік:</b> <i>2024</i><p>

## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом. Вимоги до функцій:

1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів

## Варіант 4
1. Написати функцію remove-seconds-and-reverse , яка видаляє зі списку кожен
другий елемент і обертає результат у зворотному порядку:
```lisp
CL-USER> (remove-seconds-and-reverse '(1 2 a b 3 4 d))
(D 3 A 1)
```
2. Написати функцію list-set-difference , яка визначає різницю двох множин,
заданих списками атомів:
```lisp
CL-USER> (list-set-difference '(1 2 3 4) '(3 4 5 6))
(1 2) ; порядок може відрізнятись
```
## Лістинг функції remove-seconds-and-reverse
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```bash
$ sbcl --script scripts/remove-seconds-and-reverse.lisp 
Test 1... passed
Test 2... passed
Test 3... passed
Test 4... passed
Test 4... passed
Test 5... passed
Test 6... passed
```
## Лістинг функції list-set-difference
```lisp
(defun atom-in-list (atom list)
"Check if atom is in list"
  (cond
    ((null list) nil)
    ((eql atom (car list)) t)
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
```
### Тестові набори
```lisp
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
  (check-list-set-difference "6" '() '() nil)
)
```
### Тестування
```bash
$ sbcl --script scripts/list-set-difference.lisp 
Test 1... passed
Test 2... passed
Test 3... passed
Test 4... passed
Test 5... passed
Test 6... passed
```