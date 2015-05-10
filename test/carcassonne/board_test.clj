(ns carcassonne.board_test
  (:require [clojure.test :refer :all]
            [clojure.set :refer [union select]]))

(defn edges
  ([] {:n nil :e nil :s nil :w nil})
  ([n e s w] {:n n :e e :s s :w w}))

(defn tile [id x y edge-types edges]
  { :id id :x x :y y :edge-types edge-types :edges edges })

(def board (atom #{}))
(def available-spaces (atom #{}))

(defn space [x y] { :x x :y y })

(defn surrounding-spaces [tile]
  (let [x (:x tile) y (:y tile)]
    #{(space x (- y 1))
      (space (+ x 1) y)
      (space x (+ y 1))
      (space (- x 1) y) }))

(defn same-space? [s1 s2] 
  (and (= (:x s1) (:x s2)) (= (:y s1) (:y s2))))

(defn is-space-on-board? [space]
  (nil? (some (fn [board-space] (same-space? space board-space)) @board)))

(defn add-to-available-spaces [new-spaces]
  (let [new-spaces-not-on-board (select is-space-on-board? new-spaces)]
    (reset! available-spaces (union @available-spaces new-spaces-not-on-board))))

(defn remove-from-available-spaces [space]
  (let [tiles-not-matching-space #(not (same-space? space %))]
    (reset! available-spaces (select tiles-not-matching-space @available-spaces))))

(defn add-first-tile-to-board [tile]
  (swap! board conj tile)
  (add-to-available-spaces (surrounding-spaces tile)))

(defn add-tile-to-board [tile connections]
  (swap! board conj tile)
  (add-to-available-spaces (surrounding-spaces tile))
  (remove-from-available-spaces tile))

(use-fixtures :each (fn [test-to-run] 
  (reset! board #{})
  (reset! available-spaces #{}) 
  (test-to-run)))

(deftest test-app
  (add-first-tile-to-board (tile 1 0 0 (edges :field :road :field :road) (edges)))
  (is (= 1 (count @board)))
  (is (= 4 (count @available-spaces)))
  
  (remove-from-available-spaces (space 1 0))
  (is (= 3 (count @available-spaces)))

  (remove-from-available-spaces (space 0 1))
  (is (= 2 (count @available-spaces))))

(deftest another-test
  (add-first-tile-to-board (tile 1 0 0 (edges :field :road :field :road) (edges)))
  (add-tile-to-board (tile 2 1 0 (edges :field :road :field :road) (edges)) nil)

  (is (= 2 (count @board)))    
  (is (= 6 (count @available-spaces))))

