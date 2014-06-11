(ns clog.core-test
  (:require [clojure.test :refer :all]
            [clog.core :refer :all]))

(deftest clog-test
  (testing "call with integer arguments"
    (is (= {"main#foo" 1.6}
           (clog "def foo; 1 + 1; end")))
    ))
