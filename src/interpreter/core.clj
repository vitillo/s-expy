(ns interpreter.core
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]))

(def parse
  (insta/parser
    "Program     = Form*
    <Form>       = Expression
    <Expression> = (Let / If / Lambda / Constant / Symbol / Application)
    Lambda       = Lparen <'fn'> Formals Expression Rparen
    Formals      = Lparen Symbol* Rparen
    If           = Lparen <'if'> Expression Expression Expression Rparen
    Let          = Lparen <'let'> Bindings Expression Rparen
    Application  = Lparen Expression+ Rparen
    Bindings     = Lparen Binding* Rparen
    <Binding>    = Symbol Expression
    <Constant>   = Number | Boolean
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
        body (second (rseq fun))
        env (merge (second (peek fun)) (apply hash-map (interleave formals actuals)))]
    (interp-eval env body)))

(defn- interp-eval [env [tag & body :as tree]]
  (let [eval-par (partial interp-eval env)
        eval-map (partial map eval-par)]

    (case tag
      :Program
      (interp-eval env (tree 1))

      :Application
      (let [operator (interp-eval env (tree 1))
            arguments (eval-map (subvec tree 2))]
        (if (and (ifn? operator) (not (coll? operator)))
          (apply operator arguments) ;apply builtin
          (interp-apply env operator arguments)))

      :Lambda
      (conj tree [:Env env])

      :Let
      (let [new-env (interp-eval env (tree 1))]
        (interp-eval new-env (peek tree)))

      :If
      (let [condition (interp-eval env (tree 1))
            then (interp-eval env (tree 2))
            else (interp-eval env (tree 3))]
        (if condition then else))

      :Bindings
      (let [extract #(if (zero? (mod %1 2))
                       (symbol (%2 1))
                       (interp-eval env %2))
            bindings (apply hash-map (map-indexed extract body))]
        (println body)
        (merge env bindings))

      :Symbol
      ((comp env symbol) (tree 1))

      :Number
      (Integer/parseInt (tree 1))

      :Boolean
      (Boolean/parseBoolean (tree 1)))))


(defn interp [expression]
  (let [ast (parse expression)]
    (interp-eval top-env ast)))
