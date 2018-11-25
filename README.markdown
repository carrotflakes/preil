# preil

A prolog-like logic programming library for Common Lisp.

## Usage

```lisp
;; First, you need secure some memory.
(initialize-memory 1000)

(in-world (make-world))

;; Define 'append' predicate.
(<- (append () ?xs ?xs))
(<- (append (?x . ?xs) ?ys (?x . ?zs))
    (append ?xs ?ys ?zs))

;; Join 2 lists, (1 2 3) and (4 5).
(print (solve-all ?xs
                  '(append (1 2 3) (4 5) ?xs)))
;; => ((1 2 3 4 5))

;; Enumerate all patterns separating (1 2 3).
(print (solve-all (?xs ?ys)
                  '(append ?xs ?ys (1 2 3))))
;; => ((NIL (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) NIL))

```

## Installation

```
$ ros install carrotflakes/preil
```

## APIs
- `create-world`
- `<-`
- `%-`
- `import-world`
- `solve`
- `solvep`
- `solve-1`
- `solve-all`
- `do-solve`

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2016-2017 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
