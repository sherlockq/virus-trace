(ns virus-trace
  (:require [clojure.spec.alpha :as s]))

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

(defn next-visit [visited distances]
  (key (first (sort-by val (apply dissoc distances visited)))))

(defn solve [{:keys [source target dictionary]}]
  "Solve tracing virus"
  {:solved true
   :steps  []}
  (loop [visited #{}
         distances-and-steps {:distances {source 0} :steps {source []}}
         dictionary dictionary]
    (cond
      (solved? target visited distances-and-steps)
      {:solved true
       :steps  (get-in distances-and-steps [:steps target])}
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
