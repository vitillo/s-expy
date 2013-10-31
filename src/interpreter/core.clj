(ns interpreter.core
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]))

(def parse
  (insta/parser
    "Program     = Form*
    <Form>       = Expression
    <Expression> = (Let / Constant / Symbol / Application)
    Let          = Lparen <'let'> Bindings Expression+ Rparen
    Bindings     = Lparen Binding* Rparen
    <Binding>    = Symbol Expression
    <Constant>   = Number
    Application  = Lparen Expression+ Rparen
    Symbol       = Space* #'[\\pL_$&/+~:<>|§?*-][\\pL\\p{Digit}_$&/+~.:<>|§?*-]*' Space*
    Number       = Space* #'[0-9]+' Space*
    <Lparen>     = Space* <'('> Space*
    <Rparen>     = Space* <')'> Space*
    <Space>      = <#'\\s*'>"))

(def ^:private top-env {'+ +
                        '- -
                        '/ /
                        '* *
                        'square ['x [:Application  [:Symbol "*"]  [:Symbol "x"]  [:Symbol "x"]]]})

(declare interp-eval)

(defn- interp-apply [env fun arg]
  (let [param (first fun)
        body (second fun)
        env (merge top-env {param (first arg)})]
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

      :Let
      (let [new-env (interp-eval env (fnext tree))]
        (interp-eval new-env (last tree)))

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
      (Integer/parseInt (second tree)))))

(defn interp [expression]
  (let [ast (parse expression)]
    (interp-eval top-env ast)))
