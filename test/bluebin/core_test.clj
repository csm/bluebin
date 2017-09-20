(ns bluebin.core-test
  (:require [clojure.test :refer :all]
            [bluebin.core :refer :all]))

(defrecyclable Test [a b c])

(deftest test-recyclable
  (let [t (->Test 1 2 3)
        t2 (map->Test {:a 1 :b 2 :c 3})]
    (is (= t t2))
    (is (= 1 (:a t)))
    (is (= 2 (:b t)))
    (is (= 3 (:c t)))
    (is (nil? (:d t)))
    (is (= :foo (get t :d :foo)))
    (is (= 3 (count t)))
    (is (= [:a :b :c] (keys t)))
    (is (= [1 2 3] (vals t)))
    (recycle! t)
    (is (thrown? IllegalStateException (count t)))
    (is (thrown? IllegalStateException (:a t)))
    (is (thrown? IllegalStateException (:b t)))
    (is (thrown? IllegalStateException (:c t)))
    (is (thrown? IllegalStateException (:d t)))))

(defrecyclable TestNamed [a b c] :key-ns "bluebin.core-test")

(deftest test-named
  (let [t (->TestNamed 1 2 3)
        t2 (map->TestNamed {:bluebin.core-test/a 1
                            :bluebin.core-test/b 2
                            :bluebin.core-test/c 3})]
    (is (= t t2))
    (is (= 1 (:bluebin.core-test/a t)))
    (is (= 2 (:bluebin.core-test/b t)))
    (is (= 3 (:bluebin.core-test/c t)))
    (is (nil? (:bluebin.core-test/d t)))
    (is (= :foo (get t :bluebin.core-test/d :foo)))
    (is (= 3 (count t)))
    (is (= [:bluebin.core-test/a :bluebin.core-test/b :bluebin.core-test/c] (keys t)))
    (is (= [1 2 3] (vals t)))
    (recycle! t)
    (is (thrown? IllegalStateException (count t)))
    (is (thrown? IllegalStateException (:bluebin.core-test/a t)))
    (is (thrown? IllegalStateException (:bluebin.core-test/b t)))
    (is (thrown? IllegalStateException (:bluebin.core-test/c t)))
    (is (thrown? IllegalStateException (:bluebin.core-test/d t)))))

(deftest test-rec-vec
  (let [v (->rec-vec 1 2 3 4)]
    (is (= 4 (count v)))
    (is (= 1 (first v)))
    (is (= 2 (second v)))
    (is (= 3 (nth v 2)))
    (is (= 4 (nth v 3)))
    (is (thrown? IndexOutOfBoundsException (nth v 4)))
    (recycle! v)
    (is (thrown? IllegalStateException (count v)))
    (is (thrown? IllegalStateException (first v)))
    (is (thrown? IllegalStateException (second v)))
    (is (thrown? IllegalStateException (nth v 2)))
    (is (thrown? IllegalStateException (nth v 3)))))