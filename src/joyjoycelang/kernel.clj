(ns joyjoycelang.kernel)

(defn split-vec [v, i]
  (let [split-inx (if (pos? i), i, (+ (count v) i))]
    [(subvec v 0 split-inx)
     (subvec v split-inx)]))

(declare run)

(comment
  (run [[[:b] :gimme-b] :def]))

(def joyenv
  (atom
   {:cake (fn cake [stack]
            (let [[stack', [[b], [a]]] (split-vec stack -2)]
              (conj stack', [[b] a], [a [b]])))
    :k (fn k [stack]
         (let [[stack', [[b], [a]] (split-vec stack -2)]]
           (conj stack', a)))
    :def (fn def-op [stack]
           (let [stack' (pop stack)
                 [val name] (peek stack)]
             (swap! joyenv
                    (fn [env]
                      (assoc env name
                             (fn [stack'']
                               (run stack'' val)))))
             stack'))}))

(defn step-dispatch [stack, head]
  (if (find @joyenv head), :fn, :default))

(def step nil)
(defmulti step #'step-dispatch)

(defmethod step :fn [stack, head]
  ((@joyenv head) stack))

(defmethod step :default [stack, head]
  (conj stack head))

(defn run
  ([src] (run [] src))
  ([stack src] (reduce step stack src)))
