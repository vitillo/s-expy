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
          (it "parses let bindings"
              (should-not-contain :index (pret (parse "(let (x 4) (+ x 4))"))))
          (it "parses boolean constants"
              (should-not-contain :index (pret (parse "true false"))))
          (it "parses if expressions"
              (should-not-contain :index (pret (parse "(if true true false)"))))
          (it "parses lambda expressions"
              (should-not-contain :index (pret (parse "(fn (x) (* x x))"))))
          (it "parses set! expressions"
              (should-not-contain :index (pret (parse "(let (x 3) (set! x 4))"))))
          (it "parses begin expressions"
              (should-not-contain :index (pret (parse "(begin (+ 3 3) (+ 2 2))"))))
          (it "parses define expressions"
              (should-not-contain :index (pret (parse "(define foo 42)")))))

(describe "Interpreter"
          (it "evaluates booleans"
              (should (= true (interp "true"))))
          (it "evaluates defined symbols"
              (should (= 5 (interp "(+ 2 3)"))))
          (it "evaluates arithmetic expressions"
              (should (= 30 (interp "(+ 10 (/ 10 2) (* 2 5) (- 8 3))"))))
          (it "allows to create bindings"
              (should (= 5 (interp "(let (x 3) (+ x 2))"))))
          (it "evaluates if expressions"
              (should (= true (interp "(if (> 5 3) true false)"))))
          (it "evaluates lambda expressions"
              (should-contain :Lambda (interp "(fn (x) (* x x))")))
          (it "evaluates calls to immediate functions"
              (should (= 4 (interp "((fn (x) (* x x)) 2)"))))
          (it "evaluates closures"
              (should (= 9 (interp "((let (x 3) (fn () (* x x))))"))))
          (it "evaluates set! expressions"
              (should (= 4 (interp "(let (x 3) (set! x 4))"))))
          (it "evaluates begin expressions"
              (should (= 4 (interp "(begin (+ 3 3) (+ 2 2))"))))
          (it "evaluates define expressions"
              (should (= 42 (interp "(begin (define foo 42) foo)")))))

(run-specs)
