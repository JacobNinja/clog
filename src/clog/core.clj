(ns clog.core
  (:require [clog.parser :as parser]))

(declare processors result-processors)

(defn get-type [node]
  (cond
   (parser/defn-node? node) :defn
   (parser/fixnum-node? node) :fixnum
   (parser/call-node? node) :call
   (parser/alias-node? node) :alias
   (parser/class-node? node) :class
   (parser/iter-node? node) :iter
   (parser/assignment-node? node) :assignment))

(defn fingerprint [env]
  (str (last (:class-stack env))
       "#"
       (last (:method-stack env))))

(defn- process-env [node env]
  (if-let [processor (processors (get-type node))]
    (processor node env)
    env))

(defn- process-result [node env result]
  (if-let [result-processor (result-processors (get-type node))]
    (update-in result [(fingerprint env)] #(merge-with + % (result-processor env)))
    result))

(defn process-defn [node env]
  (update-in env [:method-stack] #(conj % (parser/get-name node))))

(defn process-class [node env]
  (update-in env [:class-stack] #(conj % (parser/get-class-name node))))

(defn process-call [node env]
  (update-in env [:multiplier] #(+ % 0.2)))

(defn process-iter [node env]
  (update-in env [:multiplier] #(+ % 0.1)))

(defn result-fixnum [env]
  {:other (* (:multiplier env) 0.25)})

(defn result-alias [env]
  {:other (* (:multiplier env) 2.0)})

(defn result-iter [env]
  {:branch (* (:multiplier env) 1.0)})

(defn result-call [env]
  {:other 1.0})

(defn result-assignment [env]
  {:assignment (* (:multiplier env) 1.0)})

(def processors {:defn process-defn
                 :call process-call
                 :class process-class
                 :iter process-iter})

(def result-processors {:fixnum result-fixnum
                        :alias result-alias
                        :iter result-iter
                        :assignment result-assignment
                        :call result-call})

(defn- process-root
  ([root]
   (process-root root {:multiplier 1.0 :class-stack ["main"] :method-stack ["none"]} {}))
  ([root env]
   (process-root root env {}))
  ([root env result]
   (let [children (parser/children root)
         next-env (process-env root env)
         next-result (process-result root next-env result)]
     (if (empty? children)
       next-result
       (apply (partial merge-with (partial merge-with +))
              (cons next-result
                    (map #(process-root % next-env) children)))))))

(defn clog [rb]
  (into {}
        (map (fn [[fp scores]]
               [fp (Math/sqrt (reduce + 0 (map #(Math/pow % 2) (vals scores))))])
             (process-root (parser/parse-ruby rb)))))
