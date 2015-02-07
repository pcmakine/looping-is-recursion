(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [base acc exp]
                       (cond
                        (= exp 0) 1
                        (= exp 1) acc
                        :else (recur base (* base acc) (- exp 1))))]
  (power-helper base base exp)))



(defn last-element [a-seq]
  (let [singleton? (fn [coll]
                     (and (not (empty? coll)) (empty? (rest coll))))]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
    :else (recur (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
   (or (and (empty? a-seq) (not (empty? b-seq))) (and (empty? b-seq) (not (empty? a-seq)))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
   (empty? a-seq) nil
   (pred (first a-seq)) index
   :else (recur (+ index 1) pred (rest a-seq))
   ))
  )

(defn avg [a-seq]
  (loop [a-seq a-seq
         num_of_elements (count a-seq)
         sum 0]
    (cond
     (empty? a-seq) (/ sum num_of_elements)
     :else (recur (rest a-seq) num_of_elements (+ sum (first a-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]
  (loop [a-seq a-seq
         a-set (set [])]
    (cond
     (empty? a-seq) a-set
     :else (recur (rest a-seq) (toggle a-set (first a-seq))))))

(defn fast-fibo [n]
  (loop [prev 1
         prevprev 0
         current 2
         n n]
    (cond
     (< n 2) n
     (= current n) (+ prev prevprev)
     :else (recur (+ prev prevprev) prev (+ current 1) n)
)))


(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         ret-seq '()]
    (cond (empty? a-seq) (reverse ret-seq)
           (contains? (set ret-seq) (first a-seq)) (reverse ret-seq)
          :else (recur (rest a-seq) (conj ret-seq (first a-seq))))))

