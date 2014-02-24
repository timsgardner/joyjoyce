(ns joyjoycelang.kernel)

(defn split-vec [v, i]
  (let [split-inx (if (pos? i), i, (+ (count v) i))]
    [(subvec v 0 split-inx)
     (subvec v split-inx)]))

(declare run)

(def joyenv
  (atom
   {:dup (fn [stack] (conj stack (peek stack)))
    :def (fn def-op [stack]
           (let [[stack', [val, name]] (split-vec stack -2)]
             (swap! joyenv
                    (fn [env]
                      (assoc env name
                             (fn [stack]
                               (run (conj stack val))))))
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
