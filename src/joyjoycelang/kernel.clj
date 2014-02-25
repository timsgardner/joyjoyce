(ns joyjoycelang.kernel
  (:use [clojure.test :only [is]]))

(defn split-vec [v, i]
  (let [split-inx (if (pos? i), i, (+ (count v) i))]
    [(subvec v 0 split-inx)
     (subvec v split-inx)]))

(defn remove-nil [x]
  (if (vector? x)
    (mapv remove-nil (remove nil? x)),
    x))

(declare run)

(comment
  (run [[[:b] :gimme-b] :def]))

(def joyenv
  (atom
   {:cake (fn cake [stack]
            (let [[stack', [bv, av]] (split-vec stack -2)]
              ;; this is stupid because of destructuring crap. Means: [[b]a] [a[b]]
              (run stack', (remove-nil [(into [bv] av), (conj av bv)]))))
    :k (fn k [stack]
         (let [[stack', [bv, av]] (split-vec stack -2)]
           (run stack', (remove-nil av))))
    :def (fn def-op [stack]
           (let [stack' (pop stack)
                 [val name] (peek stack)]
             (swap! joyenv
                    (fn [env]
                      (assoc env name
                             (fn [stack'']
                               (run stack'' val)))))
             stack'))}))

(defn step [stack, head]
  (cond
   (nil? head) stack,
   (contains? @joyenv head) ((@joyenv head) stack),
   :else (conj stack head)))

(defn run
  ([src] (run [] src))
  ([stack src] (reduce step stack src)))

;; bootstrap --------------------------------------------

(run
  [[[[] :k] :zap] :def
   [[:cake :k] :dip] :def
   [[:cake [] :k] :cons] :def
   [[[[]] :dip :k] :i] :def
   [[[] :cake :dip :dip] :dup] :def])

(is (= (run [[:b] [:a] :cake]) [[[:b] :a] [:a [:b]]]))

(is (= (run [[:b] [:a] :k]) [:a]))

(is (= (run [[:a] :zap]) []))

(is (= (run [[:b] [:a] :dip]) [:a [:b]]));; <-

(is (= (run [[:b] [:a] :cons]) [[[:b] :a]]))

(is (= (run [[:a] :i]) [:a]))

(is (= (run [[:a] :dup]) [[:a] [:a]]))
