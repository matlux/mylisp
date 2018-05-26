# mylisp

Lisp interpreter written in Clojure, to experiment with new `clojure.specs` features for evaluating and conforming S-exrpessions efficiently.

## Usage

Run the REPL by executing `lein run` in the project root directory.

While in the REPL try executing the following expressions:

    (+ 1 2 3)
    ;; => 6

    (defun square (x) (* x x))
    ;; => square

    (square 5)
    ;; = > 25

    (let (x (+ 2 3)) (square x))
    ;; => 25

Enjoy!

## License

Copyright © 2014 Daniel Dinnyes

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
