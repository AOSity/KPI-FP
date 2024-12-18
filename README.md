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
(defvar *failed-tests* nil "List of failed test descriptions")

(defmacro assert-equal (expected actual &optional (message ""))
  "Assert that 'expected' and 'actual' are equal"
 `(let ((exp ,expected) (act ,actual))
    (unless (equal exp act)
      (push (format nil "Assertion failed: ~a~%Expected: ~a~%Got: ~a" ,message exp act) *failed-tests*)
    )
  )
)

(defmacro assert-true (form &optional (message ""))
  "Assert that 'form' value is non-nil"
 `(let ((val ,form))
    (unless val
      (push (format nil "Assertion failed: ~a~%Expected non-nil, got NIL" ,message) *failed-tests*)
    )
  )
)

(defun run-test (test-func)
  "Run test function which uses assert"
  (let ((*failed-tests* nil))
    (funcall test-func)
    (if *failed-tests*
      (progn
          (format t "~a FAILED ~%" test-func)
          (dolist (fail *failed-tests*) (format t "~a~%" fail))
          nil
      )
      (progn (format t "~a passed ~%" test-func) t)
    )
  )
)

(defun test-get-struct-slots ()
  "Test for 'get-struct-slots'"
  (assert-true (every #'(lambda (s) (member s project-slots)) '(id name domain release-year model-id))
               "Project slots do not contain expected fields"
  )
  (assert-true (every #'(lambda (s) (member s model-slots)) '(id name type version developer))
               "Model slots do not contain expected fields"
  )
)

(defun test-normalize-header ()
  "Test for 'normalize-header'"
  (assert-equal :NAME (normalize-header "Name") "Failed to normalize 'Name'")
  (assert-equal :RELEASE-YEAR (normalize-header " release year ") "Failed to normalize ' release year '")
  (assert-equal :MODEL-ID (normalize-header "Model ID") "Failed to normalize 'Model ID'")
  (assert-equal :DEVELOPER (normalize-header "developer") "Failed to normalize 'developer'")
)

(defun test-split-string ()
  "Test for 'split-string'"
  (assert-equal '("a" "b" "c") (split-string "a,b,c" #\,) "Splitting 'a,b,c' failed")
  (assert-equal '("one" "two" "three") (split-string "one|two|three" #\|) "Splitting 'one two three' failed")
  (assert-equal '("no-sep") (split-string "no-sep" #\,) "Should return unchanged string")
  (assert-equal '("spaces" "in" "string") (split-string "spaces in string" #\Space)
                "Splitting 'spaces in string' by space failed"
  )
)

(defun test-struct-to-hash-table ()
  "Test for 'struct-to-hash-table'"
  (let ((test-project (make-project :id 7 :name "TestProj" :domain "Testing" :release-year 2024 :model-id 5))
        (hash (struct-to-hash-table 
                (make-project :id 7 :name "TestProj" :domain "Testing" :release-year 2024 :model-id 5) project-slots
              )
      ))
    (assert-equal 7 (gethash "ID" hash) "ID field mismatch")
    (assert-equal "TestProj" (gethash "NAME" hash) "NAME field mismatch")
    (assert-equal "Testing" (gethash "DOMAIN" hash) "DOMAIN field mismatch")
    (assert-equal 2024 (gethash "RELEASE-YEAR" hash) "RELEASE-YEAR field mismatch")
    (assert-equal 5 (gethash "MODEL-ID" hash) "MODEL-ID field mismatch")
  )
)

(defun test-read-write-struct-csv ()
  "Test for writing and reading a struct to/from .csv"
  (let  ((test-file "test.csv")
         (test-data (make-model :id 88 :name "Model" :type "Neural Network" :version "1.4" :developer "TestCorp"))
        )
    (write-struct-to-csv test-file test-data model-slots)
    (let ((models (read-struct-from-csv test-file #'make-model)))
      (assert-true (and models (car models)) "No models read back from .csv")
      (let ((m (car models)))
        (assert-equal "88" (model-id m) "ID mismatch")
        (assert-equal "Model" (model-name m) "Name mismatch")
        (assert-equal "Neural Network" (model-type m) "Type mismatch")
        (assert-equal "1.4" (model-version m) "Version mismatch")
        (assert-equal "TestCorp" (model-developer m) "Developer mismatch")
      )
    )
  )
)

(defun test-select ()
  "Test for the 'select'"
  (let ((all-projects (funcall (select "projects.csv" :project)))
        (healthcare-projects (funcall (select "projects.csv" :project :domain "Healthcare")))
        (all-models (funcall (select "models.csv" :model)))
        (meditech-models (funcall (select "models.csv" :model :developer "MediTech")))
       )
       (assert-true (plusp (length all-projects)) "No projects read from file")
       (assert-true (every (lambda (p) (string= (project-domain p) "Healthcare")) healthcare-projects)
                    "Filter by domain failed."
       )
       (assert-true (plusp (length all-models)) "No models read from file")
       (assert-true (every (lambda (m) (string= (model-developer m) "MediTech")) meditech-models) 
                    "Filter by developer failed."
       )
  )
)

(defun test-print-table ()
  "Test for 'print-table'"
  (let  ((test-projects (list (make-project :id 1 :name "Proj1" :domain "Dom1" :release-year 2020 :model-id 3)
                              (make-project :id 2 :name "Proj2" :domain "Dom2" :release-year 2021 :model-id 2)
                              (make-project :id 3 :name "Proj3" :domain "Dom3" :release-year 2022 :model-id 1)
                        )
        ))
        (print-table test-projects project-slots)
        (assert-true t "print-table executed without errors")
  )
)

(defun setup-test-environment ()
  "Setup test.csv"
  (with-open-file 
    (stream "test.csv" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "id,name,type,version,developer~%")
  )
)

(defun run-all-tests ()
  "Run all tests and report results"
  (setup-test-environment)
  (let  ((results (list (run-test #'test-print-table)
                        (run-test #'test-get-struct-slots)
                        (run-test #'test-normalize-header)
                        (run-test #'test-split-string)
                        (run-test #'test-struct-to-hash-table)
                        (run-test #'test-read-write-struct-csv)
                        (run-test #'test-select)
                  )
        ))
        (if (every #'identity results)
          (format t "~%All tests passed successfully~%")
          (format t "~%SOME TESTS FAILED~%")
        )
  )
)

(run-all-tests)
```
### Тестування
```bash
$ sbcl --script db.lisp 
ID | NAME | DOMAIN | RELEASE-YEAR | MODEL-ID
1 | "Proj1" | "Dom1" | 2020 | 3
2 | "Proj2" | "Dom2" | 2021 | 2
3 | "Proj3" | "Dom3" | 2022 | 1
#<FUNCTION TEST-PRINT-TABLE> passed 
#<FUNCTION TEST-GET-STRUCT-SLOTS> passed 
#<FUNCTION TEST-NORMALIZE-HEADER> passed 
#<FUNCTION TEST-SPLIT-STRING> passed 
#<FUNCTION TEST-STRUCT-TO-HASH-TABLE> passed 
#<FUNCTION TEST-READ-WRITE-STRUCT-CSV> passed 
#<FUNCTION TEST-SELECT> passed 

All tests passed successfully
```