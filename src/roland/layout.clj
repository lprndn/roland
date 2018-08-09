;;;_ Namespace
(ns roland.layout
  (:import [no.birkett.kiwi Solver Strength Symbolics Variable Constraint Expression])
  (:require [roland.utils :refer [engrave! deftag]]
            [roland.measurements :refer [toMeasurement toPoints]])
  (:refer-clojure :rename {+ core+
                           - core-
                           * core*
                           / corediv
                           <= core<=
                           >= core>=}))


;;;_ Layouts
;;[:layout {}
;;         [:frame {:name ...}]]
;;;_ * Records and protocols

;;;_  * Frame and constrainable conversion

(defrecord Frame [name next top bottom left right width height])

(defprotocol Constrained
  (constrainable [value layout]))

(extend-protocol Constrained
  
  Number
  (constrainable [value layout]
    (double value)) 

  String
  (constrainable [value layout]
    (:value (toPoints (toMeasurement value))))
  
  clojure.lang.PersistentVector
  (constrainable [value layout]
    (get-in layout value))
  
  clojure.lang.PersistentArrayMap
  (constrainable [value layout]
    (apply (:op value)
           (map #(constrainable % layout) (:args value))))
  
  Object
  (constrainable [value layout]
    value))

;;;_  * Multiple types arithmetic

(defprotocol Multimath
  (add [x y])
  (sub [x y])
  (mult [x y])
  (div [x y])
  (<= [x] [x y])
  (>= [x] [x y])
  (neg [x])
  (strenght [x y]))

(defn + [x & ys] 
  (add x ys))

(defn - 
  ([x] (neg x))
  ([x & ys] (sub x ys)))

(defn * [x & ys]
  (mult x ys))

(defn / [x & ys]
  (div x ys))

(extend-protocol Multimath
  Number
  (add [x y]
    (apply core+ x y))
  (sub [x y]
    (apply core- x y))
  (mult [x y]
    (apply core* x y))
  (div [x y]
    (apply corediv x y))
  (neg [x]
    (- x))
  (<= 
    #_([x] #(Symbolics/lessThanOrEqualTo % x))
    ([x y] (core<= x y)))
  (>= 
    #_([x] #(Symbolics/greaterThanOrEqualTo % x))
    ([x y] (core>= x y)))
  
  no.birkett.kiwi.Constraint
  (strenght [x y]
    (.setStrength x y))
  
  Object
  (add [x y]
    {:op (fn [x y] (Symbolics/add x y)) :args (into [x] y)})
  (sub [x y]
    {:op (fn [x y] (Symbolics/subtract x y)) :args (into [x] y)})
  (mult [x y]
    {:op (fn [x y] (Symbolics/multiply x y)) :args (into [x] y)})
  (div [x y]
    {:op (fn [x y] (Symbolics/divide x y)) :args (into [x] y)})
  (neg [x]
    {:op #(Symbolics/negate %) :args [x]})
  (<= [x]
    (fn [v l] (Symbolics/lessThanOrEqualTo v (constrainable x l))))
  (>= [x]
    (fn [v l] (Symbolics/greaterThanOrEqualTo v (constrainable x l)))))

;;;_ * Functions

(defn init-frame
  [name solver]
  (let [frm (map->Frame {:name (or name (str (gensym "frame")))
                         :next nil
                         :top (Variable. "top")
                         :bottom (Variable. "bottom")
                         :left (Variable. "left")
                         :right (Variable. "right")
                         :width (Variable. "width")
                         :height (Variable. "height")})]
    (doto solver
      (.addConstraint (.setStrength (Symbolics/equals (:left frm) (Symbolics/subtract (:right frm) (:width frm))) (Strength/REQUIRED)))
      (.addConstraint (.setStrength (Symbolics/equals (:bottom frm) (Symbolics/subtract (:top frm) (:height frm))) (Strength/REQUIRED)))
      (.addConstraint (.setStrength (Symbolics/greaterThanOrEqualTo (:top frm) (:bottom frm)) (Strength/STRONG)))
      (.addConstraint (.setStrength (Symbolics/greaterThanOrEqualTo (:right frm) (:left frm)) (Strength/STRONG))))
    frm))

(defn- set-constraint! [kiwi-var value layout solver]
  (.addConstraint solver
                  (if (instance? clojure.lang.Fn value) 
                    (value kiwi-var layout)
                    (Symbolics/equals kiwi-var (constrainable value layout)))))

(defn- set-frame! [frame layout solver]
  (doseq [[key value] (select-keys frame [:top :bottom :left :right :width :height])]
    (set-constraint! (get-in layout [(frame :name) key]) value layout solver)))

(deftag layout [name]
  (let [solver (Solver.)
        els (map #(if ((second %) :name) (second %) (assoc (second %) :name (str (gensym "frame_")))) elements)
        layout-map (reduce (fn [acc x] (conj acc x)) {} 
                           (map #(let [name (% :name)]
                                   [name (init-frame name solver)])
                                els))]
    (do
      (doseq [frame els]
        (set-frame! frame layout-map solver))
      (.updateVariables solver)
      (let [frames (map (fn [[k m]] 
                          [k (apply ->Frame
                                    (map (fn [[k2 m2]]
                                           (if (instance? no.birkett.kiwi.Variable m2) 
                                             (.getValue m2) 
                                             m2)) m))]) layout-map)
            lyt {:layout (into {} frames)}]
        (engrave! lyt nil)
        lyt))))

#_(deftag layout []
(let [solver (Solver.)
      frames-group (partition 2 (interleave (map second elements) 
                                            (map #(init-frame ((second %) :name) solver) elements))) 
      frames (reduce (fn [acc frame] (into acc (set-frame! frame solver))) {} frames-group)]
  (do
    (.updateVariables solver)
    (engrave! {:layout frames} nil))))

#_(deftag frame [solver name next
                 top right bottom left width height
                 :or {name (gensym "frame")}]
    (let []
      (doseq [attrs (select-keys params [:top :right :bottom :left :width :height])])))

#_(defn frame 
    [{:keys [solver name next
             top bottom left right width height] :as options}]
    "Declares a frame's variables and add various constraints."
    (let [frm (init-frame solver)]
      (doseq [attr (select-keys options [:top :bottom :left :right :width :height])]
        (add-constraint (second attr) ((first attr) frm) solver))
      {(or (keyword name) (keyword (gensym "frame"))) (merge frm {:name name :next next})}))

#_(defn layout
    [options & frames]
    (let [solver (Solver.)
          desc (engrave! (merge options {:solver solver}) frames)]
      (do
        (.updateVariables solver)
        {:layout desc})))
