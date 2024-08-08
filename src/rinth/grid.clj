(ns rinth.grid)

(defn make-grid
  [rows cols cell-init]
  {:width cols
   :cells (cell-init rows cols)})

(defn cell
  [row col]
  {:row row
   :col col
   :north [(dec row) col]
   :south [(inc row) col]
   :east [row (inc col)]
   :west [row (dec col)]
   :links #{}})

(defn init-cells
  [rows cols]
  (vec (for [r (range rows) c (range cols)] (cell r c))))

(defn size [grid] (count (:cells grid)))

(defn cols [grid] (:width grid))

(defn rows [grid] (/ (size grid) (cols grid)))

(defn in-bounds?
  [grid row col]
  (and (<= 0 row) (< row (rows grid)) (<= 0 col) (< col (cols grid))))

(defn raw-index [grid row col] (+ (* row (cols grid)) col))

(defn cell-at
  [grid row col]
  (when (in-bounds? grid row col)
    (get-in grid [:cells (raw-index grid row col)])))

(defn reinsert
  [grid cell]
  (assoc-in grid [:cells (raw-index grid (:row cell) (:col cell))] cell))

(def opposite-dir {:north :south :south :north :west :east :east :west})

(defn link
  ([grid row col direction] (link grid row col direction true))
  ([grid row col direction bidirectional?]
   (let [cell (cell-at grid row col)
         new-grid (reinsert grid (update cell :links conj direction))]
     (if bidirectional?
       (let [[link-row link-col] (get cell direction)]
         (recur new-grid link-row link-col (opposite-dir direction) false))
       new-grid))))

(defn linked? [cell direction] (some? (get (:links cell) direction)))

(defn neighbors
  [grid cell]
  (filter (fn [dir] (apply in-bounds? grid (get cell dir)))
          [:north :south :east :west]))

(defn neighbors-set [grid cell] (apply hash-set (neighbors grid cell)))

(defn neighbor?
  [grid cell direction]
  (contains? (neighbors-set grid cell) direction))

(defn distances
  ([grid start-row start-col]
   (distances grid
              [[start-row start-col]]
              []
              (-> (repeat (size grid) -1)
                  vec
                  (assoc (raw-index grid start-row start-col) 0))))
  ([grid frontier new-frontier result]
   (cond
     (seq frontier)
     (let [[curr-row curr-col] (first frontier)
           curr-cell (cell-at grid curr-row curr-col)
           unvisited-links (->> (:links curr-cell)
                                (map (fn [dir] (get curr-cell dir)))
                                (filter (fn [[r c]]
                                          (->> (raw-index grid r c)
                                               (nth result)
                                               neg?))))
           curr-dist (nth result (raw-index grid curr-row curr-col))]
       (recur grid
              (subvec frontier 1)
              (apply conj new-frontier unvisited-links)
              (reduce (fn [res [r c]]
                        (assoc res (raw-index grid r c) (inc curr-dist)))
                      result
                      unvisited-links)))

     (seq new-frontier) (recur grid new-frontier [] result)
     :else result)))

(defn shortest-path-search
  [grid start-row start-col dists path]
  (if (= (first path) [start-row start-col])
    path
    (let [[curr-row curr-col] (first path)
          curr-cell (cell-at grid curr-row curr-col)
          min-link (->> (:links curr-cell)
                        (map (fn [dir] (get curr-cell dir)))
                        (apply min-key
                               (fn [[r c]] (nth dists (raw-index grid r c)))))]
      (recur grid start-row start-col dists (conj path min-link)))))

(defn shortest-path
  ([grid start-row start-col goal-row goal-col]
   (shortest-path-search grid
                         start-row
                         start-col
                         (distances grid start-row start-col)
                         (list [goal-row goal-col]))))
