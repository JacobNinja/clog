(ns clog.core-test
  (:require [clojure.test :refer :all]
            [clog.core :refer :all]))

(def complex
"class Test
  def foo
    something.each do |bar|
      bar += 3
    end
  end
  alias foo bar
end")

(deftest clog-test
  (testing "call with integer arguments"
    (is (= {"main#foo" 1.6}
           (clog "def foo; 1 + 1; end"))))
  (testing "complex"
    (is (= {"Test#foo" 3.8432570822155525 "Test#none" 2.0}
           (clog complex))))
  )
