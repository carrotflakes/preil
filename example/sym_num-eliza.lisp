;;;; http://qiita.com/sym_num/items/a99ad21dbeb5fabaf61d

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:preil
                  :preil-prelude
                  :split-sequence)))

(use-package :preil)


(with-world ()

  (prelude:import-definition)

  (<- (init-memory
       ((other 1)
        (computer 1)
        (sorry 1)
        (iremember 1)
        (iwant 1)
        (if 1)
        (no 1)
        (yes 1)
        (iwas 1))))

  (<- (read-memory ?pair ?memory)
    (member ?pair ?memory))
  (<- (update-memory ?pair (?pair . ?xs) ?pair* (?pair* . ?xs)))
  (<- (update-memory ?pair (?pair** . ?xs) ?pair* (?pair** . ?ys))
    (update-memory ?pair ?xs ?pair* ?ys))

  (<- (talk nil ?user-uttr ?memory* ?bot-uttr)
    (init-memory ?memory)
    (talk ?memory ?user-uttr ?memory* ?bot-uttr))

  (<- (talk ?memory ("hello" . ?) ?memory "How do you do. Please state your problem."))

  (<- (talk ?memory ?user-uttr ?memory* ?bot-uttr)
    (member "computer" ?user-uttr)
    (member (?prev ?next ?bot-uttr)
            ((1 2 "Do computer worry you?")
             (2 3 "What do you think about machines?")
             (3 4 "Why do you mention computers?")
             (4 1 "What do you think machines have to do with your problem?")))
    (update-memory (computer ?prev) ?memory (computer ?next) ?memory*))

  (<- (talk ?memory ?user-uttr ?memory ?bot-uttr)
    (member "name" ?user-uttr)
    (= ?bot-uttr "I am not interested in names"))

  (<- (talk ?memory ?user-uttr ?memory* ?bot-uttr)
    (member "sorry" ?user-uttr)
    (member (?prev ?next ?bot-uttr)
            ((1 2 "Please dont apologize.")
             (2 3 "Apologies are not necessary.")
             (3 1 "What feelings do you have when you apologize?")))
    (update-memory (sorry ?prev) ?memory (sorry ?next) ?memory*))

  (<- (talk ?memory ("i" "remember" . ?rest) ?memory* ?bot-uttr)
    (member (?prev ?next ?bot-uttr-parts)
            ((1 2 ("Do you often think of" ?rest-str "?"))
             (2 1 ("Does thinking of" ?rest-str "bring anything else to mind?"))))
    (update-memory (iremember ?prev) ?memory (iremember ?next) ?memory*)
    (format ?rest-str "~{~a~^ ~}" ?rest)
    (format ?bot-uttr "~{~a~^ ~}" ?bot-uttr-parts))

  (<- (talk ?memory ("i" "want" . ?rest) ?memory* ?bot-uttr)
    (member (?prev ?next ?bot-uttr-parts)
            ((1 2 ("What would it mean if you got" ?rest-str "."))
             (2 3 ("Why do you want" ?rest-str "."))
             (3 1 ("Suppose you got" ?rest-str "soon."))))
    (update-memory (iwant ?prev) ?memory (iwant ?next) ?memory*)
    (format ?rest-str "~{~a~^ ~}" ?rest)
    (format ?bot-uttr "~{~a~^ ~}" ?bot-uttr-parts))

  (<- (talk ?memory ("if" . ?rest) ?memory* ?bot-uttr)
    (member (?prev ?next ?bot-uttr-parts)
            ((1 2 ("Do you really think its likely that" ?rest-str "."))
             (2 3 ("Do you wish that" ?rest-str "."))
             (3 4 ("What do you think about" ?rest-str "."))
             (4 1 ("Really-- if" ?rest-str "."))))
    (update-memory (if ?prev) ?memory (if ?next) ?memory*)
    (format ?rest-str "~{~a~^ ~}" ?rest)
    (format ?bot-uttr "~{~a~^ ~}" ?bot-uttr-parts))

  (<- (talk ?memory ?user-uttr ?memory* ?bot-uttr)
    (member "no" ?user-uttr)
    (member (?prev ?next ?bot-uttr)
            ((1 2 "Why not?")
             (2 3 "You are being a bit negative.")
             (3 1 "Are you saying \"NO\" just to be negative?")))
    (update-memory (no ?prev) ?memory (no ?next) ?memory*))

  (<- (talk ?memory ?user-uttr ?memory* ?bot-uttr)
    (member "yes" ?user-uttr)
    (member (?prev ?next ?bot-uttr)
            ((1 2 "You seem quite positive.")
             (2 3 "You are sure.")
             (3 1 "I understand.")))
    (update-memory (yes ?prev) ?memory (yes ?next) ?memory*))

  (<- (talk ?memory ("i" "was" . ?rest) ?memory* ?bot-uttr)
    (member (?prev ?next ?bot-uttr-parts)
            ((1 2 ("Were you really?"))
             (2 3 ("Perhaps I already know you were" ?rest-str "."))
             (3 1 ("Why do you tell me you were" ?rest-str "now?"))))
    (update-memory (iwas ?prev) ?memory (iwas ?next) ?memory*)
    (format ?rest-str "~{~a~^ ~}" ?rest)
    (format ?bot-uttr "~{~a~^ ~}" ?bot-uttr-parts))

  (<- (talk ?memory ("i" "feel" . ?rest) ?memory ?bot-uttr)
    (format ?bot-uttr "Do you often feel ~{~a~^ ~}?" ?rest))

  (<- (talk ?memory ("i" "felt" . ?rest) ?memory ?bot-uttr)
    (format ?bot-uttr "What other feelings do you have ~{~a~^ ~}?" ?rest))

  (<- (talk ?memory ("bye") ?memory "Good bye !"))

  (<- (talk ?memory () ?memory* ?bot-uttr)
    (member (?prev ?next ?bot-uttr)
            ((1 2 "Very interesting.")
             (2 3 "I am not sure I understand you fully.")
             (3 4 "What does that suggest to you?")
             (4 5 "Please continue.")
             (5 6 "Go on.")
             (6 1 "Do you feel strongly about discussing such things?")))
    (update-memory (other ?prev) ?memory (other ?next) ?memory*))

  (<- (talk ?memory (? . ?user-uttr) ?memory* ?bot-uttr)
    (talk ?memory ?user-uttr ?memory* ?bot-uttr))


  (defun tokenize (text)
    (when (eq (aref text (1- (length text))) #\.)
      (setf text (subseq text 0 (1- (length text)))))
    (mapcar #'string-downcase
            (split-sequence:split-sequence #\space text)))

  (preil-defun make-talker ()
    (let ((memory nil))
      (lambda (text)
        (let ((user-uttr (tokenize text)))
          (destructuring-bind (memory* bot-uttr)
              (solve-1 (?memory ?bot-uttr)
                `(talk ,memory ,user-uttr ?memory ?bot-uttr))
            (setf memory memory*)
            bot-uttr)))))

  (preil-defun talk ()
    (format t "Hi!,I am Eliza.~%")
    (format t "Please talk about you~%")
    (loop
       with talker = (make-talker)
       for bot-uttr = (funcall talker (read-line))
       do (format t "~a~%" bot-uttr))))
