(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil :cl-mecab :cl-ppcre)))

(use-package '(:preil :cl-mecab))


(defun wakati (text)
	(with-mecab ("-d /usr/local/lib/mecab/dic/ipadic")
    (mapcar #'first (parse* text))))

(setf *world* (make-world))
(with-world (*world*)

  (%g (parse-integer ?string ?integer)
      (?string)
    (and (stringp ?string)
         (ppcre:scan "\\d+" ?string)
         (ret :?integer (parse-integer ?string))))
  (%g (parse-integer ?string ?integer)
      (?integer)
    (and (integerp ?integer)
         (ret :?string (write-to-string ?integer))))

  ;; append
  (<- (append () ?xs ?xs))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
      (append ?xs ?ys ?zs))

  ;; conc
  (<- (conc () ()))
  (<- (conc (() . ?xss) ?ys)
      (conc ?xss ?ys))
  (<- (conc ((?x . ?xs) . ?xss) (?x . ?ys))
      (conc (?xs . ?xss) ?ys))

  ;; parse
  (<- (parse ?xs  (?命令 :date ?日付 :time ?時間))
			(conc (?日付? ?助詞の? ?時間? ?助詞に? ?命令?) ?xs)
      (日付 ?日付? ?日付)
      (助詞の ?助詞の?)
      (時間 ?時間? ?時間)
      (助詞に ?助詞に?)
      (命令 ?命令? ?命令))
  (<- (parse ?xs  (?命令 :date ?日付))
			(conc (?日付? ?命令?) ?xs)
      (日付 ?日付? ?日付)
      (命令 ?命令? ?命令))
  (<- (parse ?xs  (?命令 :time ?時間))
			(conc (?時間? ?助詞に? ?命令?) ?xs)
      (時間 ?時間? ?時間)
      (助詞に ?助詞に?)
      (命令 ?命令? ?命令))

  (<- (助詞の ("の")))
  (<- (助詞の ()))

  (<- (助詞に ("に")))
  (<- (助詞に ()))

  (<- (日付 ("今日") today))
  (<- (日付 ("明日") tommorow))
  (<- (日付 ("明後日") day-after-tomorrow))

  (<- (時間 (?h? "時" ?m? "分") (?h ?m))
      (parse-integer ?h? ?h)
      (parse-integer ?m? ?m))
  (<- (時間 (?h? ":" ?m?) (?h ?m))
      (parse-integer ?h? ?h)
      (parse-integer ?m? ?m))
  (<- (時間 (?h? "時") (?h *))
      (parse-integer ?h? ?h))
  (<- (時間 (?m? "分") (* ?m))
      (parse-integer ?m? ?m))

  #|
  (<- (時間 (?h "時" ?m "分") (?h ?m)))
  (<- (時間 (?h ":" ?m) (?h ?m)))
  (<- (時間 (?h "時") (?h *)))
  (<- (時間 (?m "分") (* ?m)))
  |#
  (<- (命令 ("アラート") アラート))
  (<- (命令 ("アラート" "し" "て") アラート))
  (<- (命令 ("アラート" "し" "て") アラート))
  (<- (命令 ("知らせ" "て") アラート))

  (defun alert-parse (text)
    (let ((tokens (wakati text)))
      (solve-all ?result
                 `(parse ,tokens ?result))))
	)

#|
(do-solve ((?r) (format t "~{~a~}~%" ?r))
  `(parse ?r (アラート :time (1 *))))

(do-solve ((?r) (format t "~{~a~}~%" ?r))
  `(parse ?r (アラート :date tommorow :time (1 0))))
|#

(print (alert-parse "明日の1時にアラート"))

#|
1時にアラート
12時に知らせて
1時間後にアラート
明日アラート
明日アラートして
|#
