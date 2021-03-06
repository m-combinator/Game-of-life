(ns conway.core
  (:require [quil.core :as q])
  (:require [quil.middleware :as m]))

(defn flip [state]
  "Flips the state of a cell."
  (if (= :off state)
    :on
    :off))

;; Test board
(def board (-> (vec (repeat 17 (vec (repeat 17 :off))))
               (update-in [5 7] flip)
               (update-in [5 8] flip)
               (update-in [5 9] flip)
               (update-in [7 8] flip)
               (update-in [9 7] flip)
               (update-in [9 8] flip)
               (update-in [9 9] flip)))

(defn alive? [cell]
  "Tests if the cell is alive"
  (= cell :on))

(defn tick [board]
  "Updates the board according to the game's rules.
Performs only one iteration"
  (let [xcoord [-1 -1 -1  0 0  1 1 1]
        ycoord [-1  0  1 -1 1 -1 0 1]]
    (mapv vec
          (partition (count board)
                     (for [i (range (count board))
                           j (range (count board))]
                       (let [cell (get-in board [i j])
                             neighbors (map #(get-in board [(+ %1 i) (+ %2 j)])
                                            xcoord
                                            ycoord)
                             cell-living? (alive? cell)
                             living-neighbors-count (count (filter alive?
                                                                   neighbors))]
                         (cond (and cell-living?
                                    (< living-neighbors-count 2)) :off
                                    (and cell-living?
                                         (#{2 3} living-neighbors-count))
                                    :on
                                    (and cell-living?
                                         (> living-neighbors-count 3))
                                    :off
                                    (and (not cell-living?)
                                         (= living-neighbors-count 3))
                                    :on
                                    :else :off)))))))

(defn live-cells [board]
  "Returns a list of coordinates of the live cells on the board"
  (for [i (range (count board))
        j (range (count board))
        :when (alive? (get-in board [i j]))]
    [i j]))

(defn setup []
  "Sets the config for quil, and returns the initial state of the board"
  (q/smooth)
  (q/frame-rate 3)                 
  board)

(defn update [board]
  "Update function used by quil to update the board every frame"
  (tick board))

(defn draw [board]
  "Drawing function used by quil"
  (q/background 255)
  (q/fill 0)
  (doseq [[x y] (live-cells board)]
    (q/rect (* x 17) (* 17 y) 17 17)))

;;quil demo
(q/defsketch example
  :middleware [m/fun-mode]
  :setup setup
  :draw draw
  :update update
  :size [289 289])

(defn -main []
  "Main function"
  example)
