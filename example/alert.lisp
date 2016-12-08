(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil :cl-mecab)))

(use-package '(:preil :cl-mecab))


(defun wakati (text)
	(with-mecab ("-d /usr/local/lib/mecab/dic/ipadic")
    (mapcar #'first (parse* text))))

(setf *world* (make-world))
(with-world (*world*)

  ;; append
  (-- (append () _xs _xs))
  (-- (append (_x . _xs) _ys (_x . _zs))
      (append _xs _ys _zs))

  ;; conc
  (-- (conc () ()))
  (-- (conc (() . _xss) _ys)
      (conc _xss _ys))
  (-- (conc ((_x . _xs) . _xss) (_x . _ys))
      (conc (_xs . _xss) _ys))

  ;; parse
  (-- (parse _xs  (_命令 :date _日付 :time _時間))
      (日付 _日付? _日付)
      (助詞の _助詞の?)
      (時間 _時間? _時間)
      (助詞に _助詞に?)
      (命令 _命令? _命令)
			(conc (_日付? _助詞の? _時間? _助詞に? _命令?) _xs))
  (-- (parse _xs  (_命令 :date _日付))
      (日付 _日付? _日付)
      (命令 _命令? _命令)
			(conc (_日付? _命令?) _xs))
  (-- (parse _xs  (_命令 :time _時間))
      (時間 _時間? _時間)
      (助詞に _助詞に?)
      (命令 _命令? _命令)
			(conc (_時間? _助詞に? _命令?) _xs))

  (-- (助詞の ("の")))
  (-- (助詞の ()))

  (-- (助詞に ("に")))
  (-- (助詞に ()))

  (-- (日付 ("今日") today))
  (-- (日付 ("明日") tommorow))
  (-- (日付 ("明後日") day-after-tomorrow))

  (-- (時間 (_h "時" _m "分") (_h _m)))
  (-- (時間 (_h "時") (_h *)))
  (-- (時間 (_m "分") (* _m)))

  (-- (命令 ("アラート") アラート))
  (-- (命令 ("アラート" "し" "て") アラート))
  (-- (命令 ("アラート" "し" "て") アラート))
  (-- (命令 ("知らせ" "て") アラート))

  (defun alert-parse (text)
    (let ((tokens (wakati text)))
			(print tokens)
      (?all _result
            `(parse ,tokens _result))))

	(?print _r `(parse _r (アラート :date tommorow :time (1 0))))
	)

(print (alert-parse "明日の1時にアラート"))

#|
1時にアラート
12時に知らせて
1時間後にアラート
明日アラート
明日アラートして
|#
