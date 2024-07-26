(ns rinth.main
  (:require
   [clojure.string :as str]
   [mikera.image.colours :as col]))

(defn grid
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

(defn init-cells [rows cols] (into [] (for [r (range rows) c (range cols)] (cell r c))))

(defn cols [grid] (:width grid))

(defn rows [grid] (/ (count (:cells grid)) (cols grid)))

(defn in-bounds? [grid row col] (and (<= 0 row) (< row (rows grid)) (<= 0 col) (< col (cols grid))))

(defn- raw-index [grid row col] (+ (* row (:width grid)) col))

(defn cell-at
  [grid row col]
  (when (in-bounds? grid row col) (get-in grid [:cells (raw-index grid row col)])))

(defn reinsert [cell grid] (assoc-in grid [:cells (raw-index grid (:row cell) (:col cell))] cell))

(def opposite {:north :south :south :north :west :east :east :west})

(defn link
  ([grid cell direction] (link grid cell direction true))
  ([grid cell direction bidirectional?]
   (let [new-grid (reinsert (update cell :links conj direction) grid)]
     (if bidirectional?
       (link new-grid (apply cell-at new-grid (get cell direction)) (opposite direction) false)
       new-grid))))

(defn unlink
  ([grid cell direction] (unlink grid cell direction true))
  ([grid cell direction bidirectional?]
   (let [new-grid (reinsert (update cell :links disj direction) grid)]
     (if bidirectional?
       (unlink new-grid (apply cell-at new-grid (get cell direction)) (opposite direction) false)
       new-grid))))

(defn linked?
  [grid cell1 cell2]
  (some? (some #{cell2} (map #(apply cell-at grid (get cell1 %)) (:links cell1)))))


(defn neighbors
  [grid cell]
  (filter (fn [dir] (apply in-bounds? grid (get cell dir))) [:north :south :east :west]))

(defn rand-cell [grid] (cell-at grid (rand-int (rows grid)) (rand-int (cols grid))))

(defn size [grid] (count (:cells grid)))

(defn binary-tree
  [grid]
  (reduce (fn [g cell]
            (let [path-options (filter #{:north :east} (neighbors g cell))]
              (if (seq path-options) (link g cell (rand-nth path-options)) g)))
          grid
          (:cells grid)))

(defn sidewinder
  [grid]
  (first
   (reduce
    (fn [[g r] cell]
      (let [run (conj r (:col cell))
            eastern-boundary? (not (some #{:east} (neighbors grid cell)))
            northern-boundary? (not (some #{:north} (neighbors grid cell)))
            end-run? (or eastern-boundary? (and (not northern-boundary?) (rand-nth [true false])))]
        (if end-run?
          [(if northern-boundary? g (link g (cell-at g (:row cell) (rand-nth run)) :north)) []]
          [(link g cell :east) run])))
    [grid []]
    (:cells grid))))

(defn- render-cell
  [cell]
  [(str "   " (if (get (:links cell) :east) " " "|"))
   (str (if (get (:links cell) :south) "   " "---") "+")])

(defn- render-row
  [row]
  (reduce (fn [[top bottom] cell]
            (let [[cell-top cell-bottom] (render-cell cell)]
              [(str top cell-top) (str bottom cell-bottom)]))
          ["|" "+"]
          row))

(defn string-render
  [grid]
  (str/join \newline
            (cons (str "+" (apply str (repeat (cols grid) "---+")))
                  (mapcat render-row (partition (cols grid) (:cells grid))))))

(def algorithm-lookup {:binary-tree binary-tree :sidewinder sidewinder})

(defn run
  [{:keys [rows cols algorithm]}]
  (let [algo (algorithm-lookup algorithm)]
    (if algo
      (-> (grid rows cols init-cells)
          algo
          string-render)
      (throw (ex-info "[ERROR] Unknown algorithm" {:algorithm (name algorithm)})))))

(defn- line-low
  [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        yi 1
        yi (if (< dy 0) -1 yi)
        dy (if (< dy 0) (- dy) dy)
        d0 (- (* 2 dy) dx)]
    (->> [x0 y0 d0]
         (iterate (fn [[x y d]] [(inc x) (if (< 0 d) (+ y yi) y)
                                 (if (< 0 d) (+ (* 2 (- dy dx)) d) (+ (* 2 dy) d))]))
         (take-while (fn [[x _ _]] (<= x x1)))
         (map butlast))))

(defn- line-high
  [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        xi 1
        xi (if (< dx 0) -1 xi)
        dx (if (< dx 0) (- dx) dx)
        d0 (- (* 2 dx) dy)]
    (->> [x0 y0 d0]
         (iterate (fn [[x y d]] [(if (< 0 d) (+ x xi) x) (inc y)
                                 (if (< 0 d) (+ (* 2 (- dx dy)) d) (+ (* 2 dx) d))]))
         (take-while (fn [[_ y _]] (<= y y1)))
         (map butlast))))

(defn line
  [x0 y0 x1 y1]
  (if (< (abs (- y1 y0)) (abs (- x1 x0)))
    (if (< x0 x1) (line-low x0 y0 x1 y1) (line-low x1 y1 x0 y0))
    (if (< y0 y1) (line-high x0 y0 x1 y1) (line-high x1 y1 x0 y0))))

(defn update-image-pixel
  [image x y color]
  (assoc-in image [:pixels (+ (* (:width image) y) x)] color))

(defn as-string
  [image]
  (let [{:keys [width pixels]} image]
    (->> pixels
         (map (fn [p] (if (<= p col/darkGray) \. \@)))
         (partition width)
         (map str/join)
         (str/join \newline))))



(defn cli-entry [opts] (println (run opts)))

(defn -main [args] (println (run args)))

