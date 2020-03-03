(ns virus-trace
  (:require [clojure.spec.alpha :as s]))


(defn load-dictionary []
  (with-open [rdr (clojure.java.io/reader "wordlist.txt")]
    (reduce conj [] (line-seq rdr))))

(defn neighbour? [string1 string2]
  (if (= (count string1) (count string2))
    (= 1
       (count
         (filter
           false?
           (map = string1 string2))))
    false))

(defn- neighbours [start dictionary]
  (filter (partial neighbour? start) dictionary))

(s/def ::distances-and-steps (s/keys :opt-un [::distances ::steps]))

(defn update-point [point distance-to-update steps distances-and-steps]
  {:pre  [(string? point)
          (number? distance-to-update)
          (seqable? steps)
          (s/valid? ::distances-and-steps distances-and-steps)]
   :post [(s/valid? ::distances-and-steps %)]}

  (let [current-distance (get-in distances-and-steps [:distances point])]
    (if (or (nil? current-distance) (< distance-to-update current-distance))
      (-> distances-and-steps
          (assoc-in [:distances point] distance-to-update)
          (assoc-in [:steps point] steps))
      distances-and-steps)))

(defn visit [{:keys [start dictionary distances steps]}]
  (let [neighbours-of-start (neighbours start dictionary)
        distances-to-start (get distances start)
        steps-to-start (get steps start)
        updated-distance-steps (reduce #(update-point
                                          %2
                                          (+ 1 distances-to-start)
                                          (conj steps-to-start start)
                                          %1)
                                       {:distances distances
                                        :steps     steps}
                                       neighbours-of-start)]
    updated-distance-steps))

(defn solved? [target-point visited {distances :distances}]
  (and (contains? distances target-point)
       (= (get distances target-point)
          (apply min (vals (apply dissoc distances visited))))))

(defn solved-for-distance-one? [target-point {distances :distances}]
  "In this special case, since proximate distance is always one, as long as a distance is found it's the best solution"
  (contains? distances target-point))

(defn next-visit [visited distances]
  (key (first (sort-by val (apply dissoc distances visited)))))

(defn solve [{:keys [source target dictionary]}]
  "Solve tracing virus"
  {:solved true
   :steps  []}
  (loop [visited #{}
         distances-and-steps {:distances {source 0} :steps {source []}}
         dictionary (filter #(= (count source) (count %)) dictionary)]
    (when-let [one-solution (get-in distances-and-steps [:steps target])]
      (printf "A possible answer found: %s\n" one-solution))
    (cond
      (solved-for-distance-one? target distances-and-steps)
      (do
        (printf "Visited: %s; Known transforms: %s\n" (count visited) (count (:distances distances-and-steps)))
        {:solved true
         :steps  (get-in distances-and-steps [:steps target])})
      (= (count visited) (count (:distances distances-and-steps)))
      {:solved false}
      :else
      (let [to-visit (next-visit visited (:distances distances-and-steps))]

        (recur
          (conj visited to-visit)
          (visit (merge {:start      to-visit
                         :dictionary dictionary}
                        distances-and-steps))
          dictionary)))))


(defn -main [& args]
  (if-not (= 2 (count args))
    (throw (Exception. "Must provide source word and target word as arguments"))
    (time (println (solve {
                           :source     (first args)
                           :target     (second args)
                           :dictionary (load-dictionary)})))))