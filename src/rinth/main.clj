(ns rinth.main
  (:require
   [mikera.image.colours :as col]
   [rinth.display :as display]
   [rinth.grid :as grid]))

(defn binary-tree
  [grid]
  (reduce
   (fn [g [row col]]
     (let [path-options (filter #(grid/neighbor? g (grid/cell-at g row col) %)
                                [:north :east])]
       (if (seq path-options) (grid/link g row col (rand-nth path-options)) g)))
   grid
   (for [row (range (grid/rows grid)) col (range (grid/cols grid))] [row col])))

(defn- sidewinder-step
  [[g curr-run] [row col]]
  (let [run (conj curr-run [row col])
        eastern-boundary? (not
                           (grid/neighbor? g (grid/cell-at g row col) :east))
        northern-boundary? (not
                            (grid/neighbor? g (grid/cell-at g row col) :north))
        end-run? (or eastern-boundary?
                     (and (not northern-boundary?) (rand-nth [true false])))]
    (if end-run?
      [(if northern-boundary?
         g
         (let [[link-row link-col] (rand-nth run)]
           (grid/link g link-row link-col :north))) []]
      [(grid/link g row col :east) run])))

(defn sidewinder
  [grid]
  (first (reduce sidewinder-step
                 [grid []]
                 (for [row (range (grid/rows grid))
                       col (range (grid/cols grid))]
                   [row col]))))

(defn aldous-broder
  [grid]
  (loop [g grid
         [row col] [(rand-int (grid/rows grid)) (rand-int (grid/cols grid))]
         visited-count 1]
    (if (< visited-count (grid/size g))
      (let [next-dir (->> (grid/cell-at g row col)
                          (grid/neighbors g)
                          rand-nth)
            [next-row next-col] (get (grid/cell-at g row col) next-dir)
            visited? (-> (grid/cell-at g next-row next-col)
                         :links
                         seq)]
        (recur (if visited? g (grid/link g row col next-dir))
               [next-row next-col]
               (if visited? visited-count (inc visited-count))))
      g)))

(def algorithm-lookup {:binary-tree #'binary-tree :sidewinder #'sidewinder})

(comment
  (defn run
    [{:keys [rows cols algorithm]}]
    (if-let [algo (algorithm-lookup algorithm)]
      (-> (grid/make-grid rows cols grid/init-cells)
          algo
          display/show)
      (throw (ex-info "[ERROR] Unknown algorithm"
                      {:algorithm (name algorithm)})))))

(comment
  (defn run
    [{:keys [rows cols algorithm]}]
    (if-let [algo (algorithm-lookup algorithm)]
      (let [grid (algo (grid/make-grid rows cols grid/init-cells))
            longest-path (grid/longest-path grid)]
        (display/show-image
         (display/image-with-path-from-grid
          grid
          longest-path
          10
          col/white
          col/black
          col/cyan)))
      (throw (ex-info "[ERROR] Unknown algorithm"
                      {:algorithm (name algorithm)})))))

(comment
  "Save a grid as a png file"
  (-> (grid/make-grid 20 20 grid/init-cells)
      sidewinder
      (display/image-from-grid 20 col/magenta col/cyan)
      (display/save-image "my-maze.png")))

(defn run
  "Show a grid on-screen as a Jframe"
  [{:keys [rows cols algorithm]}]
  (let [transform (algorithm-lookup algorithm)
        grid (transform (grid/make-grid rows cols grid/init-cells))
        longest-path (grid/longest-path grid)]
    (-> grid
        (display/image-with-path-from-grid longest-path
                                           20
                                           col/orange
                                           col/green
                                           col/magenta)
        display/show-image)))

(comment
  "Show a grid on-screen as a Jframe, convienence method with `sensible defaults`"
  (-> (grid/make-grid 20 20 grid/init-cells)
      sidewinder
      display/show))

(comment
  (run {:rows 8 :cols 8 :algorithm :sidewinder})
)

(defn cli-entry [opts] (println (run opts)))

(defn -main [args] (println (run args)))
