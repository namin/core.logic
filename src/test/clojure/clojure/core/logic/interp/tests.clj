(ns clojure.core.logic.interp.tests
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is] :as l]
        [clojure.core.logic.interp :as i]
        clojure.test))

;; NOTES
;; 1. we care about fresh
;; 2. we care about conde
;; 3. if encounter -inc, we force it
;; 4. we never call goals we've seen before
;; 5. we can test whether two goal are the same, their classes will match
;;    this works for regular relations, ones that don't using matching sugar

(deftest test-tracer-1
  (is (= (tree (bind (bind (tracer) s#) s#))
        [:goal :goal]))
  (is (= (tree (bind (tracer) (fresh [x] s# s#)))
        [[:fresh '(x) :goal :goal]]))
  (is (= (tree (bind (tracer) (fresh [x] (fresh [y] s#) s#)))
        [[:fresh '(x) [:fresh '(y) :goal] :goal]]))
  (is (= (tree (bind (tracer) (fresh [x] (conde [s# s#] [s# s#]) s#)))
        [[:fresh '(x) [:conde [[:goal :goal] [:goal :goal]]] :goal]])))

(defn- bar []
  (conde
    [s#]
    [s#]))
(defn- foo []
  (fresh [x y]
    s#
    (bar)))

(deftest test-tracer-2-relations
  (is (= (tree (bind (tracer) (foo)))
        [[:fresh '(x y) :goal [:conde [[:goal] [:goal]]]]])))

(defn- aloop []
  (conde
    [s#]
    [(aloop)]))

(deftest test-tracer-3-recursive-goals
  (is (= (tree (bind (tracer) (aloop)))
        [[:conde [[:goal] [[:goal :seen]]]]])))
