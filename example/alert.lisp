(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil :cl-mecab :cl-ppcre)))

(use-package '(:preil :cl-mecab))


(defun normalize (text)
  (ppcre:regex-replace-all
   "[０-９]" text
   (lambda (target-string start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end match-end reg-starts reg-ends))
     (format nil "~a" (code-char (+ (char-code (aref target-string match-start)) #.(- (char-code #\0) (char-code #\０))))))))

(defun wakati (text)
	(with-mecab ("-d /usr/local/lib/mecab/dic/ipadic")
    (mapcar #'first (parse* (normalize text)))))

(with-world ()

  (%g (parse-integer ?string ?integer)
      (?string)
    (and (stringp ?string)
         (ppcre:scan "\\d+" ?string)
         (ret :?integer (parse-integer ?string))))
  (%g (parse-integer ?string ?integer)
      (?integer)
    (and (integerp ?integer)
         (ret :?string (write-to-string ?integer))))

  ;; member
  (<- (member ?x (?x . ?)))
  (<- (member ?x (? . ?y))
      (member ?x ?y))

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
			(conc (?日付? ?助詞に? ?命令?) ?xs)
      (日付 ?日付? ?日付)
      (助詞に ?助詞に?)
      (命令 ?命令? ?命令))
  (<- (parse ?xs  (?命令 :time ?時間))
			(conc (?時間? ?助詞に? ?命令?) ?xs)
      (時間 ?時間? ?時間)
      (助詞に ?助詞に?)
      (命令 ?命令? ?命令))
  (<- (parse ?xs  (?命令 :after ?after))
			(conc (?時間後? ?助詞に? ?命令?) ?xs)
      (時間後 ?時間後? ?after)
      (助詞に ?助詞に?)
      (命令 ?命令? ?命令))

  (<- (助詞の ("の")))
  (<- (助詞の ()))

  (<- (助詞に ("に")))
  (<- (助詞に ()))

  (<- (日付 ("今日") today))
  (<- (日付 ("明日") tommorow))
  (<- (日付 ("明後日") day-after-tomorrow))
  (<- (日付 (?d? "日") (:date ?d))
      (parse-integer ?d? ?d))
  (<- (日付 (?曜日) (:dow ?曜日))
      (member ?曜日
              ("月曜日"
               "火曜日"
               "水曜日"
               "木曜日"
               "金曜日"
               "土曜日"
               "日曜日")))

  (<- (時間 (?h? "時" ?ms? "分") (?h ?m))
      (parse-integer ?h? ?h)
      (parse-integer ?m? ?m))
  (<- (時間 (?h? ":" ?m?) (?h ?m))
      (parse-integer ?h? ?h)
      (parse-integer ?m? ?m))
  (<- (時間 (?h? "時") (?h *))
      (parse-integer ?h? ?h))
  (<- (時間 (?m? "分") (* ?m))
      (parse-integer ?m? ?m))

  (<- (時間後 (?h? "時間" ?m? "分" "後") (?h ?m))
      (parse-integer ?h? ?h)
      (parse-integer ?m? ?m))
  (<- (時間後 (?h? "時間" "後") (?h *))
      (parse-integer ?h? ?h))
  (<- (時間後 (?m? "分" "後") (* ?m))
      (parse-integer ?m? ?m))

  (<- (命令 ?xs alert)
      (member ?xs
              (("アラート")
               ("アラート" "し" "て")
               ("通知")
               ("通知" "し" "て")
               ("お知らせ")
               ("お知らせ" "し" "て")
               ("知らせ" "て"))))

  (preil-defun alert-parse (text)
    (print text)
    (let ((tokens (wakati text)))
      (print (solve-all ?result
                        `(parse ,tokens ?result)))))
	)

(alert-parse "明日の1時にアラート")
(alert-parse "明日の1時にアラートして")
(alert-parse "日曜日の1時にアラートして")
(alert-parse "火曜日に通知して")
(alert-parse "11日にアラート")
(alert-parse "２分後アラート")
(alert-parse "10時間後にアラート")
(alert-parse "10時間２分後にアラート")

#|
1時にアラート
12時に知らせて
1時間後にアラート
明日アラート
明日アラートして
|#
