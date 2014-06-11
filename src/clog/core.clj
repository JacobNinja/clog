(ns clog.core
  (:require [clog.parser :as parser]))

(declare processors result-processors)

(defn get-type [node]
  (cond
   (parser/defn-node? node) :defn
   (parser/fixnum-node? node) :fixnum
   (parser/call-node? node) :call))

(defn fingerprint [env]
  (str (last (get env :class-stack ["main"]))
       "#"
       (last (get env :method-stack ["none"]))))

(defn- process-env [node env]
  (if-let [processor (processors (get-type node))]
    (processor node env)
    env))

(defn- process-result [node env result]
  (if-let [result-processor (result-processors (get-type node))]
    (update-in result [(fingerprint env)] (result-processor env))
    result))

(defn process-defn [node env]
  (update-in env [:method-stack] #(cons (parser/get-name node) %)))

(defn process-call [node env]
  (update-in env [:multiplier] #(+ % 0.2)))

(defn result-fixnum [env]
  (fn [score]
    (+ (* (:multiplier env) 0.25) (or score 0))))

(defn result-defn [env]
  (fn [score]
    (+ (* (:multiplier env) 1.0) (or score 0))))

(def processors {:defn process-defn
                 :call process-call})

(def result-processors {:fixnum result-fixnum
                        :defn result-defn})

(defn- process-tree
  ([tree]
   (process-tree tree {:multiplier 1.0} {}))
  ([tree env result]
   (cond
    (empty? tree) result
    (seq? (first tree)) (merge result
                               (process-tree (first tree) env result)
                               (process-tree (rest tree) env result))
    :else (let [next-env (process-env (first tree) env)]
            (process-tree (rest tree)
                          next-env
                          (process-result (first tree) next-env result))))
   ))

(defn clog [rb]
  (process-tree (parser/parse-tree rb)))
