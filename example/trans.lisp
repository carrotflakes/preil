;;;; 日英翻訳プログラム
;;;; c.f. https://az-prolog.com/manual/sample/sample_Language_002.html
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil
                  :preil-prelude
                  :split-sequence)))

(use-package :preil)


;; DCG https://en.wikipedia.org/wiki/Definite_clause_grammar
;; (--> (a hoge) (b fuga) (c piyo))
;; => (<- (a hoge ?x0 ?x2) (b fuga ?x0 ?x1) (c piyo ?x1 ?x2))
(defmacro --> (head &rest terms)
  (let ((variables (loop
                      repeat (1+ (length terms))
                      collect (gensym "?x"))))
    `(<- (,@head ,(nth 0 variables) ,(nth (length terms) variables))
       ,@(loop
            for term in terms
            for i from 0
            collect `(,@term ,(nth i variables) ,(nth (1+ i) variables))))))

(with-world ()

  (prelude:import-definition)

  ;; DCG の補助述語
  (<- (w ?word ?xs ?xs*)
      (= ?xs (?word . ?xs*)))

  ;; 英語構文
  ;; 通常文
  (--> (s (s ?np ?vp))
       (np ?np)
       (vp ?vp))

  ;; 命令文
  (--> (s (s ?vp))
       (vp ?vp))

  ;; 名詞句
  (--> (np (np ?n ?np))
       (n ?n)
       (np ?np))
  (--> (np (np ?det ?noun))
      (det ?det)
      (n ?noun))
  (--> (np (np ?noun))
      (n ?noun))

  ;; 動詞句
  (--> (vp (vp ?vi))
      (vi ?vi))
  (--> (vp (vp ?vi ?pp))
      (vi ?vi)
      (pp ?pp))
  (--> (vp (vp ?vt ?np))
      (vt ?vt)
      (np ?np))
  (--> (vp (vp ?vt ?np ?pp))
      (vt ?vt)
      (np ?np)
      (pp ?pp))

  ;; 前置詞句
  (--> (pp (pp ?p ?np))
      (p ?p)
      (np ?np))

  ;; 終端節
  (<- (n (n ?word) (?word . ?x) ?x)
      (member ?word ("i" "you" "arrow" "flies" "time" "like")))

  (--> (det (det "a")) (w "a"))
  (--> (p (p "like")) (w "like"))

  ;; 動詞
  (<- (vt (vt ?word) (?word . ?x) ?x)
      (member ?word ("time" "like")))
  (<- (vi (vi ?word) (?word . ?x) ?x)
      (member ?word ("flies" "like")))

  ;;; 日本語構文
  ;; 通常文
  (--> (js (s ?np ?vp))
       (jnp ?np)
       (w "は")
       (jvp ?vp))

  ;; 命令文
  (--> (js (s ?vp))
       (jvp ?vp))

  ;; 名詞句
  (--> (jnp (np ?n ?np))
       (jn ?n)
       (w "の")
       (jnp ?np))
  (--> (jnp (np ?det ?noun))
       (jdet ?det)
       (jn ?noun))
  (--> (jnp (np ?noun))
       (jn ?noun))

  ;; 動詞句
  (--> (jvp (vp ?vi))
       (jvi ?vi))
  (--> (jvp (vp ?vi ?pp))
       (jpp ?pp)
       (jvi ?vi))

  (--> (jvp (vp ?vt ?np))
       (jnp ?np)
       (w "を")
       (jvt ?vt))
  (--> (jvp (vp ?vt ?np ?pp))
       (jnp ?np)
       (w "を")
       (jpp ?pp)
       (jvt ?vt))

  ;; 前置詞句
  (--> (jpp (pp ?p ?np))
       (jnp ?np)
       (jp ?p))

  ;; 終端節
  (<- (jn (n ?word) (?jword . ?x) ?x)
    (member (?word ?jword)
            (("i" "私")
             ("you" "あなた")
             ("arrow" "矢")
             ("flies" "蝿")
             ("time" "時")
             ("like" "好み"))))

  (--> (jdet (det "a")) (w "ひとつの"))
  (--> (jp (p "like")) (w "のように"))

  ;; 動詞
  (<- (jvt (vt ?word) (?jword . ?x) ?x)
    (member (?word ?jword)
            (("time" "計る")
             ("like" "好む")
             ("flies" "揚げる"))))
  (<- (jvi (vi ?word) (?jword . ?x) ?x)
    (member (?word ?jword)
            (("like" "好む")
             ("flies" "飛ぶ")
             ("flies" "揚がる"))))


  (defun tokenize (text)
    (when (eq (aref text (1- (length text))) #\.)
      (setf text (subseq text 0 (1- (length text)))))
    (mapcar #'string-downcase
            (split-sequence:split-sequence #\space text)))

  (preil-defun en-jp (en-text)
    (solve-1 ?jp-tokens
      `(s ?imi ,(tokenize en-text) ())
      `(js ?imi ?jp-tokens ())))

  (preil-defun jp-en (jp-text)
    (solve-1 ?en-tokens
      `(js ?imi ,(tokenize jp-text) ())
      `(s ?imi ?en-tokens ())))
  )


(print (en-jp "time flies like a arrow"))
(print (en-jp "I like you"))
(print (jp-en "時 は ひとつの 矢 のように 飛ぶ"))
