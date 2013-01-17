(ns clojure.core.logic.interp
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

(declare tracer trace-subst?)

;; to remove thunks
(defn unwrap-goal [g]
  (cond
    (trace-subst? g) g
    (= (mk-exp g) :inc) (recur (g))
    :else g))

(defprotocol ISearchTree
  (tree [this]))

(deftype TraceSubstitutions [_tree seen]
  ISearchTree
  (tree [this] _tree)

  IBind
  (bind [this g]
    (if (seen (class g))
      (tracer (conj _tree [:goal :seen]) seen)
      (let [exp (mk-exp g)
            sub-tree (cond
                       (= exp :fresh) [exp (map :oname (mk-vars g))]
                       (= exp :conde) []
                       :else :goal)
             sub-s (tracer sub-tree (conj seen (class g)))
             new-tree (if (= exp :conde)
                        (conj _tree [:conde (-> (g sub-s) unwrap-goal tree)])
                        (conj _tree (-> (g sub-s) unwrap-goal tree)))]
        (tracer new-tree))))

  IMPlus
  (mplus [this f]
    (let [s (unwrap-goal (f))]
      (tracer (into [_tree] [(tree s)])))))

(defn tracer
  ([] (tracer [] #{}))
  ([tree] (tracer tree #{}))
  ([tree seen]
    (TraceSubstitutions. tree seen)))

(defn trace-subst? [x]
  (instance? TraceSubstitutions x))
