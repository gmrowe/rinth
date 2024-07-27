(ns rinth.display
  (:require
   [clojure.string :as str]
   [mikera.image.core :as img]
   [mikera.image.colours :as col]
   [rinth.grid :as grid]))

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
            (cons (str "+" (apply str (repeat (grid/cols grid) "---+")))
                  (mapcat render-row
                   (partition (grid/cols grid) (:cells grid))))))

(defn- line-low
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

(defn- line-high
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
  (if (and (< x (:width image))
           (< y (/ (count (:pixels image)) (:width image))))
    (assoc-in image [:pixels (+ (* (:width image) y) x)] color)
    image))

(defn maze-pixels
  [grid cell-size]
  (mapcat (fn [{:keys [row col] :as cell}]
            (let [x0 (* cell-size col)
                  y0 (* cell-size row)
                  x1 (+ x0 cell-size)
                  y1 (+ y0 cell-size)
                  western-boundary?
                  (not (contains? (grid/neighbors-set grid cell) :west))

                  northern-boundary?
                  (not (contains? (grid/neighbors-set grid cell) :north))]
              (concat (if western-boundary? (line x0 y0 x0 y1) [])
                      (if northern-boundary? (line x0 y0 x1 y0) [])
                      (if (grid/linked? cell :east) [] (line x1 y0 x1 y1))
                      (if (grid/linked? cell :south) [] (line x0 y1 x1 y1)))))
   (:cells grid)))

(defn image-from-grid
  [grid cell-size bg-color wall-color]
  (let [image (empty-image (inc (* (grid/cols grid) cell-size))
                           (inc (* (grid/rows grid) cell-size))
                           bg-color)
        maze (maze-pixels grid cell-size)]
    (reduce (fn [img [x y]] (update-image-pixel img x y wall-color))
            image
            maze)))

(defn show-image
  [image]
  (let [imz (img/new-image (:width image) (:height image))]
    (img/set-pixels imz (int-array (:pixels image)))
    (img/show imz)
    imz))

(def default-cell-size 10)

(defn show
  ([grid] (show grid default-cell-size col/white col/black))
  ([grid cell-size bg-color wall-color]
   (show-image (image-from-grid grid cell-size bg-color wall-color))))
