(ns rinth.main
  (:require [clojure.string :as str]))

(defn grid [rows cols cell-init] {:width cols, :cells (cell-init rows cols)})

(defn cell
  [row col]
  {:row row,
   :col col,
   :north [(dec row) col],
   :south [(inc row) col],
   :east [row (inc col)],
   :west [row (dec col)],
   :links #{}})

(defn init-cells
  [rows cols]
  (into [] (for [r (range rows) c (range cols)] (cell r c))))

(defn cols [grid] (:width grid))

(defn rows [grid] (/ (count (:cells grid)) (cols grid)))

(defn in-bounds?
  [grid row col]
  (and (<= 0 row) (< row (rows grid)) (<= 0 col) (< col (cols grid))))

(defn- raw-index [grid row col] (+ (* row (:width grid)) col))

(defn cell-at
  [grid row col]
  (when (in-bounds? grid row col)
    (get-in grid [:cells (raw-index grid row col)])))

(defn reinsert
  [cell grid]
  (assoc-in grid [:cells (raw-index grid (:row cell) (:col cell))] cell))

(def opposite {:north :south, :south :north, :west :east, :east :west})

(defn link2
  ([grid row col direction] (link2 grid row col direction true))
  ([grid row col direction bidirectional?]
   (let [new-grid (update-in grid
                             [:cells (raw-index grid row col) :links]
                             conj
                             direction)]
     (if bidirectional?
       (let [[link-row link-col] (get direction (cell-at grid row col))]
         (recur new-grid link-row link-col (opposite direction) false))
       new-grid))))

(defn link
  ([grid cell direction] (link grid cell direction true))
  ([grid cell direction bidirectional?]
   (let [new-grid (reinsert (update cell :links conj direction) grid)]
     (if bidirectional?
       (link new-grid
             (apply cell-at new-grid (get cell direction))
             (opposite direction)
             false)
       new-grid))))

(defn unlink
  ([grid cell direction] (unlink grid cell direction true))
  ([grid cell direction bidirectional?]
   (let [new-grid (reinsert (update cell :links disj direction) grid)]
     (if bidirectional?
       (unlink new-grid
               (apply cell-at new-grid (get cell direction))
               (opposite direction)
               false)
       new-grid))))

(defn linked?
  [grid cell1 cell2]
  (some? (some #{cell2}
               (map #(apply cell-at grid (get cell1 %)) (:links cell1)))))


(defn neighbors
  [grid cell]
  (keep (fn [dir] (apply cell-at grid (get cell dir)))
        [:north :south :east :west]))

(defn rand-cell
  [grid]
  (cell-at grid (rand-int (rows grid)) (rand-int (cols grid))))

(defn size [grid] (count (:cells grid)))

(defn binary-tree
  [grid]
  (reduce (fn [g cell]
            (let [neighbors (filter #(apply in-bounds? g (get cell %))
                              [:north :east])]
              (if (seq neighbors) (link g cell (rand-nth neighbors)) g)))
    grid
    (:cells grid)))

(defn string-render
  [grid]
  (let [outer [(str "+" (apply str (repeat (cols grid) "---+")))]
        inner (mapcat (fn [row]
                        (reduce (fn [[top bottom] cell]
                                  [(str top
                                        "   "
                                        (if (get (:links cell) :east) " " "|"))
                                   (str
                                     bottom
                                     (if (get (:links cell) :south) "   " "---")
                                     "+")])
                          ["|" "+"]
                          row))
                (partition (cols grid) (:cells grid)))]
    (str/join \newline (concat outer inner))))


(defn run
  [_args]
  (-> (grid 4 4 init-cells)
      binary-tree
      string-render))

(defn -main [& args] (println (run args)))
