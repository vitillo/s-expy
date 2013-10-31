(ns interpreter.core-spec
  (:require [speclj.core :refer :all]
            [interpreter.core :refer :all]
            [clojure.pprint :refer :all]))

(defn pret [expression]
  (pprint expression)
  expression)


(describe "Parser"
          (it "parses defined symbols"
              (should-not-contain :index (pret (parse "(+ 2 3)"))))
          (it "ignores whitespace"
              (should-not-contain :index (pret (parse " ( +  10  2  ( / 10 2) (* 2 5 ) (- 8 3))"))))
          (it "parses arithmetic expressions on integers"
              (should-not-contain :index (pret (parse "(+ 10 2 (/ 10 2) (* 2 5) (- 8 3))"))))
          (it "parses calls to user-defined functions"
              (should-not-contain :index (pret (parse "(square 4)"))))
          (it "parses let bindings"
              (should-not-contain :index (pret (parse "(let (x 4) (square 4))")))))

(describe "Interpreter"
          (it "evaluates defined symbols"
              (should (= 5 (interp "(+ 2 3)"))))
          (it "evaluates arithmetic expressions"
              (should (= 30 (interp "(+ 10 (/ 10 2) (* 2 5) (- 8 3))"))))
          (it "allows to call functions"
              (should (= 16 (interp "(square 4)"))))
          (it "allows to create bindings"
              (should (= 5 (interp "(let (x 3) (+ x 2))")))))

(run-specs)
