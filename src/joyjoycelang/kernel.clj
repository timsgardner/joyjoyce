(ns joyjoycelang.kernel)

(declare run)

(comment
  (run [[[:b] :gimme-b] :def]))

(def joyenv
  (atom
   {:dup (fn [stack] (conj stack (peek stack)))
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
