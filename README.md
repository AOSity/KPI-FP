<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент:</b> <i>Горбуль Андрій Олександрович КВ-11</i><p>
<p align="right"><b>Рік:</b> <i>2024</i><p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом.
База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
## Варіант 4
База даних - Проєкти із застосуванням ШІ<br>
Тип записів - Структура<br>
## Лістинг реалізації завдання
```lisp
(defstruct model
  id
  name
  type
  version
  developer
)

(defstruct project
  id
  name
  domain
  release-year
  model-id
)

(defun get-struct-slots (struct-name)
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (find-class struct-name)))
)

(defvar project-slots (get-struct-slots 'project))
(defvar model-slots (get-struct-slots 'model))

(defun normalize-header (header)
  "Normalize 'header' to keyword"
  (intern 
    (string-upcase 
      (substitute #\- #\Space (string-trim '(#\Space #\Tab #\Newline #\Return) header))
    ):keyword
  )
)

(defun split-string (string separator)
  "Split a 'string' into substrings by 'separator'"
  (let ((start 0) (result nil))
    (loop for end = (position separator string :start start)
          while end
          do (push (subseq string start end) result)
             (setf start (1+ end))
    )
    (push (subseq string start) result)
    (nreverse result)
  )
)

(defun write-struct-to-csv (filepath struct slots)
  "Write 'struct' to a .csv file at 'filepath'"
  (with-open-file (stream filepath :direction :output :if-exists :append :if-does-not-exist :create)
    (let  ((values 
            (mapcar 
              (lambda (slot)
                      (let* 
                        ((symbol (intern (string-upcase (symbol-name slot)) (find-package :common-lisp-user)))
                         (value (slot-value struct symbol))
                        )
                        (cond
                          ((null value) "")
                          ((stringp value) value)
                          (t (princ-to-string value))
                        )
                      )
              )
              slots
            )
          ))
          (format stream "~{~a~^,~}~%" values)
    )
  )
)

(defun read-struct-from-csv (filepath struct-type)
  "Read .csv file and return list of structures of type 'struct-type'"
  (with-open-file (stream filepath :direction :input)
    (let* ( (headers (mapcar #'normalize-header (split-string (read-line stream) #\,)))
            (rows (loop for line = (read-line stream nil)
                    while line 
                      collect (split-string line #\,)
                  )
            )
          )
          (mapcar 
            (lambda 
              (row) 
              (apply struct-type 
                (mapcan
                  (lambda (header value) (list header (string-trim '(#\Space #\Tab #\Newline #\Return) value))) 
                  headers
                  row
                )
              )
            )
            rows
          )
    )
  )
)

(defun select (filepath key &rest filters)
  "Return lambda that returns selected records as structures. 
  'key' specifies the type (:project or :model). 'filters' are keyword-value pairs for filtering"
  (let* ((struct-map '((:project . make-project) (:model . make-model)))
         (constructor (cdr (assoc key struct-map)))
        )
        (lambda (&key &allow-other-keys)
          (let ((data (read-struct-from-csv filepath constructor)))
            (if filters
              (let* ((filter-pairs (loop for (filter-key value) on filters by #'cddr collect (cons filter-key value))))
                (remove-if-not
                  (lambda (item)
                    (every  (lambda (filter)
                              (let* ((field (slot-value item (intern (symbol-name (car filter)))))
                                     (filter-value (cdr filter))
                                    )
                                (string= (write-to-string field) (write-to-string filter-value))
                              )
                            )
                            filter-pairs
                    )
                  )
                  data
                )
              )
              data
            )
          )
        )
  )
)

(defun struct-to-hash-table (struct slots)
  "Convert 'struct' into hash table"
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (slot slots)
      (let* ((slot-symbol (intern (string-upcase (symbol-name slot)) (find-package :common-lisp-user))) 
             (value (slot-value struct slot-symbol))
            )
            (setf (gethash (string-upcase (symbol-name slot)) hash) value)
      )
    )
    hash
  )
)

(defun print-table (structures slot-names)
  "Print table for list of 'structrures' with specified 'slot-names'"
  (let 
    ( (headers (mapcar (lambda (slot) (string-upcase (symbol-name slot))) slot-names))
      (rows (mapcar (lambda (struct)
                      (mapcar (lambda (slot)
                                  (let ((value (slot-value struct slot)))
                                       (if value (write-to-string value) "")
                                  )
                              )
                              slot-names
                      )
                    )
                    structures
            )
      )
    )
    (format t "~{~a~^ | ~}~%" headers)
    (dolist (row rows)
      (format t "~{~a~^ | ~}~%" row)
    )
  )
)
```
### Тестові набори та утиліти
```lisp
;;; Лістинг реалізації утилітних тестових функцій та тестових наборів
```
### Тестування
```lisp
;;; Виклик і результат виконання тестів
```