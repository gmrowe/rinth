(ns rinth.display
  (:require
   [clojure.string :as str]
   [mikera.image.core :as img]
   [mikera.image.colours :as col]
   [rinth.grid :as grid]))

(defn- string-render-cell
  [cell content-fn]
  [(str (content-fn cell) (if (get (:links cell) :east) " " "|"))
   (str (if (get (:links cell) :south) "   " "---") "+")])

(defn- string-render-row
  [row content-fn]
  (reduce (fn [[top bottom] cell]
            (let [[cell-top cell-bottom] (string-render-cell cell content-fn)]
              [(str top cell-top) (str bottom cell-bottom)]))
          ["|" "+"]
          row))

(defn- string-render-grid
  [grid content-fn]
  (str/join \newline
            (cons (str "+" (apply str (repeat (grid/cols grid) "---+")))
                  (mapcat #(string-render-row % content-fn)
                   (partition (grid/cols grid) (:cells grid))))))

(defn string-render [grid] (string-render-grid grid (constantly "   ")))

(defn string-render-with-distance
  [grid]
  (let [dists (grid/distances grid 0 0)
        render-fn (fn [cell]
                    (let [index (grid/raw-index grid (:row cell) (:col cell))
                          cell-distance (nth dists index)]
                      (format " %-2s" cell-distance)))]
    (string-render-grid grid render-fn)))

(Integer/toString 22 36)

(defn string-render-with-path
  [grid start-row start-col goal-row goal-col]
  (let [path (grid/shortest-path grid start-row start-col goal-row goal-col)
        tree (reduce (fn [m [i path]] (assoc-in m path i))
                     {}
                     (map-indexed vector path))
        render-fn (fn [cell]
                    (if-let [dist (get-in tree [(:row cell) (:col cell)])]
                      (format " %-2s" (Integer/toString dist 36))
                      "   "))]
    (string-render-grid grid render-fn)))

(defn line-low
  [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        yi 1
        yi (if (< dy 0) -1 yi)
        dy (if (< dy 0) (- dy) dy)
        d0 (- (* 2 dy) dx)]
    (->> [x0 y0 d0]
         (iterate
          (fn [[x y d]] [(inc x) (if (< 0 d) (+ y yi) y)
                         (if (< 0 d) (+ (* 2 (- dy dx)) d) (+ (* 2 dy) d))]))
         (take-while (fn [[x _ _]] (<= x x1)))
         (map butlast))))

(defn line-high
  [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        xi 1
        xi (if (< dx 0) -1 xi)
        dx (if (< dx 0) (- dx) dx)
        d0 (- (* 2 dx) dy)]
    (->> [x0 y0 d0]
         (iterate
          (fn [[x y d]] [(if (< 0 d) (+ x xi) x) (inc y)
                         (if (< 0 d) (+ (* 2 (- dx dy)) d) (+ (* 2 dx) d))]))
         (take-while (fn [[_ y _]] (<= y y1)))
         (map butlast))))

(defn line
  [x0 y0 x1 y1]
  (if (< (abs (- y1 y0)) (abs (- x1 x0)))
    (if (< x0 x1) (line-low x0 y0 x1 y1) (line-low x1 y1 x0 y0))
    (if (< y0 y1) (line-high x0 y0 x1 y1) (line-high x1 y1 x0 y0))))

(defn empty-image
  [width height color]
  {:width width
   :height height
   :pixels (vec (repeat (* width height) color))})

(defn update-image-pixel
  [image x y color]
  (if (and (< x (:width image)) (< y (:height image)))
    (assoc-in image [:pixels (+ (* (:width image) y) x)] color)
    image))

(defn maze-pixels
  [grid cell-size]
  (mapcat (fn [{:keys [row col] :as cell}]
            (let [x0 (* cell-size col)
                  y0 (* cell-size row)
                  x1 (+ x0 cell-size)
                  y1 (+ y0 cell-size)]
              (concat
               (if (grid/neighbor? grid cell :west) [] (line x0 y0 x0 y1))
               (if (grid/neighbor? grid cell :north) [] (line x0 y0 x1 y0))
               (if (grid/linked? cell :east) [] (line x1 y0 x1 y1))
               (if (grid/linked? cell :south) [] (line x0 y1 x1 y1)))))
   (:cells grid)))

(defn cell-pixels
  [cell-size row col]
  (let [x0 (* cell-size col)
        y0 (* cell-size row)
        x1 (+ x0 cell-size)
        y1 (+ y0 cell-size)]
    (for [y (range y0 y1) x (range x0 x1)] [x y])))

(defn pixel
  [color x y]
  {:color color
   :x x
   :y y})

(defn image-from-grid
  [grid cell-size bg-color wall-color]
  (let [image (empty-image (inc (* (grid/cols grid) cell-size))
                           (inc (* (grid/rows grid) cell-size))
                           bg-color)
        maze (maze-pixels grid cell-size)]
    (reduce (fn [img [x y]] (update-image-pixel img x y wall-color))
            image
            maze)))

(defn image-with-path-from-grid
  [grid path cell-size bg-color wall-color path-color]
  (let [image (empty-image (inc (* (grid/cols grid) cell-size))
                           (inc (* (grid/rows grid) cell-size))
                           bg-color)
        path-pixels (->> path
                         (mapcat #(apply cell-pixels cell-size %))
                         (map #(apply pixel path-color %)))
        wall-pixels (map #(apply pixel wall-color %)
                         (maze-pixels grid cell-size))]
    (reduce (fn [img {:keys [x y color]}] (update-image-pixel img x y color))
            image
            (concat path-pixels wall-pixels))))

(defn show-image
  [image]
  (let [imz (img/new-image (:width image) (:height image))]
    (img/set-pixels imz (int-array (:pixels image)))
    (img/show imz)
    imz))

(defn save-image
  [image path]
  (let [imz (img/new-image (:width image) (:height image))]
    (img/set-pixels imz (int-array (:pixels image)))
    (img/save imz path)))

(def default-cell-size 10)

(defn show
  ([grid] (show grid default-cell-size col/white col/black))
  ([grid cell-size bg-color wall-color]
   (show-image (image-from-grid grid cell-size bg-color wall-color))))
