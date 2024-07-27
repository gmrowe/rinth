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
     (let [path-options (filter #{:north :east} (grid/neighbors g cell))]
       (if (seq path-options) (grid/link g cell (rand-nth path-options)) g)))
   grid
   (:cells grid)))

(defn sidewinder
  [grid]
  (first (reduce (fn [[g r] cell]
                   (let [run (conj r (:col cell))
                         eastern-boundary? (not (grid/neighbor? g cell :east))
                         northern-boundary? (not (grid/neighbor? g cell :north))
                         end-run? (or eastern-boundary?
                                      (and (not northern-boundary?)
                                           (rand-nth [true false])))]
                     (if end-run?
                       [(if northern-boundary?
                          g
                          (grid/link g
                                     (grid/cell-at g (:row cell) (rand-nth run))
                                     :north)) []]
                       [(grid/link g cell :east) run])))
                 [grid []]
                 (:cells grid))))


(def algorithm-lookup {:binary-tree binary-tree :sidewinder sidewinder})

(defn run
  [{:keys [rows cols algorithm]}]
  (let [algo (algorithm-lookup algorithm)]
    (if algo
      (-> (grid/make-grid rows cols grid/init-cells)
          algo
          display/string-render)
      (throw (ex-info "[ERROR] Unknown algorithm"
                      {:algorithm (name algorithm)})))))

(comment
  (-> (grid/make-grid 20 20 grid/init-cells)
      sidewinder
      (display/show 40 col/blue col/yellow)))

(defn cli-entry [opts] (println (run opts)))

(defn -main [args] (println (run args)))

