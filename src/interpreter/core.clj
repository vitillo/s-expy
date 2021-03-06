(ns interpreter.core
  (:require [instaparse.core :as insta]))

(def parse
  (insta/parser
    "Program     = Form*
    <Form>       = Expression
    <Expression> = (Let / If / Set / Define / Begin / Lambda / Constant / Symbol / Application)
    Lambda       = Lparen <'fn'> Formals Expression Rparen
    Formals      = Lparen Symbol* Rparen
    If           = Lparen <'if'> Expression Expression Expression Rparen
    Set          = Lparen <'set!'> Symbol Expression Rparen
    Define       = Lparen <'define'> Symbol Expression Rparen
    Begin        = Lparen <'begin'> Expression+ Rparen
    Let          = Lparen <'let'> Bindings Expression Rparen
    Application  = Lparen Expression+ Rparen
    Bindings     = Lparen Binding* Rparen
    <Binding>    = Symbol Expression
    <Constant>   = Number | Boolean
    Symbol       = Space* #'[\\pL_$&/=+~:<>|§?*-][\\pL\\p{Digit}_$&/=+~.:<>|§?*-]*' Space*
    Number       = Space* #'[0-9]+' Space*
    Boolean      = Space* ('true' | 'false') Space*
    <Lparen>     = Space* <'('> Space*
    <Rparen>     = Space* <')'> Space*
    <Space>      = <#'\\s*'>"))

(defn- env-create
  ([bindings]
   (atom (conj bindings {:outer nil})))
  ([bindings outer]
   (atom (conj bindings {:outer outer}))))

(defn- env-find [env key]
  (if (= env nil)
    nil
    (or (@env key) (env-find (@env :outer) key))))

(defn- env-update [env key value]
  (cond
    (= env nil) nil
    (@env key) (swap! env #(conj % {key value}))
    :else (env-update (@env :outer) key value)))

(defn- env-define [env key value]
  (swap! env #(conj % {key value})))

(def ^:private top-env
  (env-create {'+ +
               '- -
               '/ /
               '* *
               '> >
               '< <
               '= =
               '>= >=
               '<= <=
               'not not}))

(declare interp-eval)

(defn- interp-apply [env fun actuals]
  (let [formals (map (comp symbol second) (rest (second fun)))
        body (second (rseq fun))
        env (env-create (apply hash-map (interleave formals actuals))
                        (second (peek fun)))]
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

      :Begin
      (let [values (map (partial interp-eval env) (subvec tree 1))]
        (last values))

      :If
      (let [condition (interp-eval env (tree 1))]
        (if condition
          (interp-eval env (tree 2))
          (interp-eval env (tree 3))))

      :Set
      (let [sym (symbol (get-in tree [1 1]))
            value (interp-eval env (tree 2))]
        (env-update env sym value)
        value)

      :Define
      (let [value (interp-eval env (tree 2))]
        (env-define env (symbol (get-in tree [1 1])) value)
        value)

      :Bindings
      (let [extract #(if (zero? (mod %1 2))
                       (symbol (%2 1))
                       (interp-eval env %2))
            bindings (apply hash-map (map-indexed extract body))]
        (env-create bindings env))

      :Symbol
      (env-find env (symbol (tree 1)))

      :Number
      (Integer/parseInt (tree 1))

      :Boolean
      (Boolean/parseBoolean (tree 1)))))

(defn interp [expression]
  (let [ast (parse expression)]
    (interp-eval top-env ast)))
