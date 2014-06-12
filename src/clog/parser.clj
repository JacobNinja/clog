(ns clog.parser
  (:import (org.jrubyparser Parser CompatVersion)
           (org.jrubyparser.parser ParserConfiguration)
           (java.io StringReader)))

(defn children [n]
  (.childNodes n))

(defn defn-node? [node]
  (instance? org.jrubyparser.ast.DefnNode node))

(defn fixnum-node? [node]
  (instance? org.jrubyparser.ast.FixnumNode node))

(defn alias-node? [node]
  (instance? org.jrubyparser.ast.AliasNode node))

(defn class-node? [node]
  (instance? org.jrubyparser.ast.ClassNode node))

(defn iter-node? [node]
  (instance? org.jrubyparser.ast.IterNode node))

(defn assignment-node? [node]
  (instance? org.jrubyparser.ast.DAsgnNode node))

(defn get-name [n]
  (when (instance? org.jrubyparser.ast.INameNode n)
    (.getName n)))

(defn get-class-name [n]
  (get-name (.getCPath n)))

(defn call-node? [n]
  (or (instance? org.jrubyparser.ast.VCallNode n)
      (instance? org.jrubyparser.ast.FCallNode n)
      (instance? org.jrubyparser.ast.CallNode n)))

(defn parse-ruby [rb]
  (let [parser (Parser.)
        config (ParserConfiguration. 0 CompatVersion/RUBY1_9)]
    (.parse parser "" (StringReader. rb) config)))
