(ns interpreter.core
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]))

(def parse
  (insta/parser
    "Program     = Form*
    <Form>       = Expression
    <Expression> = (Let / If / Lambda / Constant / Symbol / Application)
    Lambda       = Lparen <'fn'> Formals Expression+ Rparen
    Formals      = Lparen Symbol* Rparen
    If           = Lparen <'if'> Expression Expression Expression Rparen
    Let          = Lparen <'let'> Bindings Expression+ Rparen
    Bindings     = Lparen Binding* Rparen
    <Binding>    = Symbol Expression
    <Constant>   = Number | Boolean
    Application  = Lparen Expression+ Rparen
    Symbol       = Space* #'[\\pL_$&/+~:<>|§?*-][\\pL\\p{Digit}_$&/+~.:<>|§?*-]*' Space*
    Number       = Space* #'[0-9]+' Space*
    Boolean      = Space* ('true' | 'false') Space*
    <Lparen>     = Space* <'('> Space*
    <Rparen>     = Space* <')'> Space*
    <Space>      = <#'\\s*'>"))

(def ^:private top-env {'+ +
                        '- -
                        '/ /
                        '* *
                        '> >
                        '< <
                        '= =
                        '>= >=
                        '<= <=
                        'not not})


(declare interp-eval)

(defn- interp-apply [env fun actuals]
  (let [formals (map (comp symbol second) (rest (second fun)))
        body (last fun)
        env (merge top-env (apply hash-map (interleave formals actuals)))]
    (interp-eval env body)))

(defn- interp-eval [env tree]
  (let [eval-par (partial interp-eval env)
        eval-map (partial map eval-par)]
    (case (first tree)
      :Program
      (interp-eval env (second tree))

      :Application
      (let [operator (interp-eval env (second tree))
            arguments (eval-map (nnext tree))]
        (if (and (ifn? operator) (not (coll? operator)))
          (apply operator arguments) ;apply builtin
          (interp-apply env operator arguments)))

      :Lambda
      tree

      :Let
      (let [new-env (interp-eval env (fnext tree))]
        (interp-eval new-env (last tree)))

      :If
      (let [condition (interp-eval env (second tree))
            then (interp-eval env (nth tree 2))
            else (interp-eval env (nth tree 3))]
        (if condition then else))

      :Bindings
      (let [bindings (apply hash-map (map-indexed #(if (= 0 (rem %1 2))
                                                     ((comp symbol second) %2)
                                                     (interp-eval env %2))
                                                  (rest tree)))]
        (merge env bindings))

      :Symbol
      (do
        ((comp env symbol) (second tree)))

      :Number
      (Integer/parseInt (second tree))

      :Boolean
      (Boolean/parseBoolean (second tree)))))


(defn interp [expression]
  (let [ast (parse expression)]
    (interp-eval top-env ast)))

(pprint (interp "(fn (x) (* x x))"))
