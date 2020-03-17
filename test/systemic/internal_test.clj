(ns systemic.internal-test
  (:require [systemic.internal :as sut]
            [clojure.test :as t :refer [deftest testing is]]))


(deftest extract-kwargs-test
  (testing "simple extraction"
    (is (= {:foo '((+ 1 2 3))
            :bar '((println "Hello") (- 1 2 3))}
           (sut/extract-kwargs #{:foo :bar :baz}
                               '(:foo (+ 1 2 3) :bar (println "Hello") (- 1 2 3))))))

  (testing "aliased extraction"
    (is (= {:foo '((+ 1 2 3))
            :bar '((println "Hello") (- 1 2 3))}
           (sut/extract-kwargs #{:foo [:bar :baz]}
                               '(:foo (+ 1 2 3) :bar (println "Hello") (- 1 2 3)))))))
