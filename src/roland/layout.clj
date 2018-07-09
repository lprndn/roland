;;;_ Namespace
(ns roland.layout
  (:import [no.birkett.kiwi Solver Strength Symbolics Variable Constraint Expression])
  (:require [roland.utils :refer [engrave! deftag]]
            [roland.measurements :refer [toMeasurement toPoints]])
  (:refer-clojure :exclude [+ - =]))


;;;_ Layouts
;;[:layout {}
;;         [:frame {:name ...}]]
;;;_ * Records and protocols

(defrecord Frame [name next top bottom left right width height])

(defprotocol Constrained
  (constrainable [value kiwi-variable]))

(extend-protocol Constrained
  Number
  (constrainable [value]
    (fn [kiwi-var]
      (double value))) 

  String
  (constrainable [value]
    (fn [kiwi-var]
      (:value (toPoints (toMeasurement value)))))
  
  clojure.lang.IFn
  (constrainable [value])
  
  Object
  (constrainable [value]
    value))

;;;_ * Functions

;;[1 2] -> [1 2] -> [1]
;;      -> [1] -> [1]
;;
(defn init-frame
  [name solver]
  (let [frm (map->Frame {:name (or name (gensym "frame"))
                         :next nil
                         :top (Variable. "top")
                         :bottom (Variable. "bottom")
                         :left (Variable. "left")
                         :right (Variable. "right")
                         :width (Variable. "width")
                         :height (Variable. "height")})]
    (doto solver
      (.addConstraint (.setStrength (Symbolics/equals (:right frm) (Symbolics/add (:bottom frm) (:width frm))) (Strength/REQUIRED)))
      (.addConstraint (.setStrength (Symbolics/equals (:top frm) (Symbolics/add (:bottom frm) (:height frm))) (Strength/REQUIRED))))
    frm))

(defn set-constraint! [constraint solver]
  (.addConstraint solver constraint))

(defn- set-frame! [layout]
  (fn [frame+dummy solver]
    (let [[frame dummy] frame+dummy]
      (doseq [[key value] (select-keys frame [:top :bottom :left :right :width :height])]
        (set-constraint! ((constrainable value) (dummy key)) solver))
      {(keyword (frame :name)) dummy})))

(defn frm [& value-path]
  (fn [layout-map]
    (get-in layout-map value-path)))

(deftag layout []
  (let [solver (Solver.)
        layout-map (apply hash-map (interleave (map #(init-frame ((second %) :name) solver))
                                               elements))
        set-frame! (set-frame! layout-map)]))

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
