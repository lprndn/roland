(ns roland.shapes
  (:import [org.apache.pdfbox.pdmodel PDPageContentStream])
  (:require [roland.utils :refer [engrave! deftag !context]]
            [roland.measurements :refer [toMeasurement toPoints]]
            [roland.colors :refer [ink]]))


(def kyklos (* 4 (/ (- (Math/sqrt 2) 1) 3)))


(defn stroke-color
  ([{:keys [content-stream color] :as options}]
   (.setStrokingColor (@!context :content-stream) (ink color))
   {:s-color color})

  ([{:keys [content-stream color] :as options}
    & elements]
   (doto content-stream
     .saveGraphicsState
     (.setStrokingColor (ink color)))
   (engrave! nil elements)
   (.restoreGraphicsState (@!context :content-stream))
   {:s-color color}))

(defn fill-color
  ([{:keys [content-stream color] :as options}]
   (.setNonStrokingColor (@!context :content-stream) (ink color))
   {:f-color color})

  ([{:keys [content-stream color] :as options}
    & elements]
   (doto content-stream
     .saveGraphicsState
     (.setNonStrokingColor (ink color)))
   (engrave! nil elements)
   (.restoreGraphicsState (@!context :content-stream))
   {:f-color color}))

(deftag rectangle [content-stream x y width height
                   :or {x 100
                        y 100
                        width 100
                        height 100}]
  (doto (@!context :content-stream)
    (fn [x] (.addRect x (map #(-> % toMeasurement toPoints :value) [x y width height])))
    #_(apply .addRect (map #(-> % toMeasurement toPoints :value) [x y width height]))
    .stroke)
  nil)

#_(defn rectangle
    [{:keys [content-stream x y width height] :as options}
     & elements]
    (doto (@!context :content-stream)
      (fn [x] (.addRect x (map #(-> % toMeasurement toPoints :value) [x y width height])))
      #_(apply .addRect (map #(-> % toMeasurement toPoints :value) [x y width height]))
      .stroke)
    nil)

(defn circle
  [{:keys [content-stream x y radius] :as options}
   & elements]
  (let [k (* radius kyklos)
        x1 (- x radius) 
        y2 (+ y radius)
        x3 (+ x radius)
        y4 (- y radius)]
    (doto (@!context :content-stream)
      (.moveTo x1 y)
      (.curveTo x1 (+ k y) (- x k) y2 x y2)
      (.curveTo (+ k x) y2 x3 (+ k y) x3 y)
      (.curveTo x3 (- y k) (+ k x) y4 x y4)
      (.curveTo (- x k) y4 x1 (- y k) x1 y)
      (.fillAndStroke))
    nil))

(defn curve 
  [{:keys [content-stream] :as options}
   & elements]
  (doto (@!context :content-stream)))

(defn line
  [{:keys [content-stream x1 y1 x2 y2] :as options}
   & elements]
  (doto (@!context :content-stream)
    (.moveTo x1 y1)
    (.lineTo x2 y2)
    .stroke)
  nil)

(defn polygon
  [{:keys [content-stream xs ys] :as options}
   & elements]
  (do
    (.moveTo (@!context :content-stream) (first xs) (first ys))
    (map (fn [x y] (.lineTo (@!context :content-stream) x y)) (next xs) (next ys))
    nil))

