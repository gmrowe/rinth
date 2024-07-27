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

(def opposite {:north :south :south :north :west :east :east :west})

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

(defn linked? [cell direction] (some? (get (:links cell) direction)))

(defn neighbors
  [grid cell]
  (filter (fn [dir] (apply in-bounds? grid (get cell dir)))
          [:north :south :east :west]))

(defn neighbors-set [grid cell] (apply hash-set (neighbors grid cell)))

(defn neighbor?
  [grid cell direction]
  (contains? (neighbors-set grid cell) direction))

(defn rand-cell
  [grid]
  (cell-at grid (rand-int (rows grid)) (rand-int (cols grid))))

(defn size [grid] (count (:cells grid)))

