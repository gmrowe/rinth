(ns rinth.main
  (:require
   [clojure.string :as str]
   [mikera.image.colours :as col]
   [rinth.display :as display]
   [rinth.grid :as grid]))

(defn binary-tree
  [grid]
  (reduce
   (fn [g cell]
     (let [path-options (filter #(grid/neighbor? g cell %) [:north :east])]
       (if (seq path-options) (grid/link g cell (rand-nth path-options)) g)))
   grid
   (:cells grid)))

(defn- sidewinder-step
  [[g r] cell]
  (let [run (conj r [(:row cell) (:col cell)])
        eastern-boundary? (not (grid/neighbor? g cell :east))
        northern-boundary? (not (grid/neighbor? g cell :north))
        end-run? (or eastern-boundary?
                     (and (not northern-boundary?) (rand-nth [true false])))]
    (if end-run?
      [(if northern-boundary?
         g
         (grid/link g (apply grid/cell-at g (rand-nth run)) :north)) []]
      [(grid/link g (grid/cell-at g (:row cell) (:col cell)) :east) run])))

(defn sidewinder
  [grid]
  (first (reduce sidewinder-step [grid []] (:cells grid))))


(def algorithm-lookup {:binary-tree #'binary-tree :sidewinder #'sidewinder})

(defn run
  [{:keys [rows cols algorithm]}]
  (let [algo (algorithm-lookup algorithm)]
    (if algo
      (-> (grid/make-grid rows cols grid/init-cells)
          algo
          display/show)
      (throw (ex-info "[ERROR] Unknown algorithm"
                      {:algorithm (name algorithm)})))))

(comment
  "Save a grid as a png file"
  (-> (grid/make-grid 20 20 grid/init-cells)
      sidewinder
      (display/image-from-grid 20 col/magenta col/cyan)
      (display/save-image "my-maze.png")))

(comment
  "Show a grid on-screen as a Jframe"
  (-> (grid/make-grid 20 20 grid/init-cells)
      sidewinder
      (display/image-from-grid 20 col/black col/cyan)
      display/show-image))

(comment
  "Show a grid on-screen as a Jframe, convienence method with `sensible defaults`"
  (-> (grid/make-grid 20 20 grid/init-cells)
      sidewinder
      display/show))

(defn cli-entry [opts] (println (run opts)))

(defn -main [args] (println (run args)))

