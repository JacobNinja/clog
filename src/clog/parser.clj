(ns clog.parser
  (:import (org.jrubyparser Parser CompatVersion)
           (org.jrubyparser.parser ParserConfiguration)
           (java.io StringReader))
  (:refer-clojure :exclude [parents]))

(defn- children? [_] true)

(defn- children [n]
  (.childNodes n))

(defn make-tree [root]
  (if-not (seq? root)
    (tree-seq children? children root)
    root))

(defn defn-node? [node]
  (instance? org.jrubyparser.ast.DefnNode node))

(defn fixnum-node? [node]
  (instance? org.jrubyparser.ast.FixnumNode node))

(defn get-name [n]
  (when (instance? org.jrubyparser.ast.INameNode n)
    (.getName n)))

(defn call-node? [n]
  (or (instance? org.jrubyparser.ast.VCallNode n)
      (instance? org.jrubyparser.ast.FCallNode n)
      (instance? org.jrubyparser.ast.CallNode n)))

(defn parse-ruby [rb]
  (let [parser (Parser.)
        config (ParserConfiguration. 0 CompatVersion/RUBY1_9)]
    (.parse parser "" (StringReader. rb) config)))

(defn parse-tree [rb]
  (make-tree (parse-ruby rb)))
