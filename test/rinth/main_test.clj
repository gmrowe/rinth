(ns rinth.main-test
  (:require [clojure.test :refer [testing is deftest]]))

(deftest initial-test
  (testing "This test will fail, and be deleted" (is (= 3 (+ 1 1)))))
