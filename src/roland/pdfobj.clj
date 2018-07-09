(ns roland.pdfobj
  (:import [org.apache.pdfbox.pdmodel PDPage PDPageContentStream] 
           [org.apache.pdfbox.pdmodel.common PDRectangle])
  (:require [roland.utils :refer [engrave! !context deftag]]
            [roland.measurements :as m]))


(defn- rectangle
  "Creates a PDRectangle from x y width height values"
  [x
   y
   width
   height]
  (PDRectangle. x y width height))

(deftag page
  [pdoc width height bleed trim
   :or {width "210mm"
        height "297mm"
        bleed "5mm"
        trim "3mm"}]
  (let [pdoc (@!context :pdoc)
        b (-> bleed m/toMeasurement m/toPoints :value)
        c (-> trim m/toMeasurement m/toPoints :value)
        w (-> width m/toMeasurement m/toPoints :value)
        h (-> height m/toMeasurement m/toPoints :value)
        mediabox (map (partial + (* 2 c)) [w h])
        bleedbox (map (partial + (* 2 b)) [w h])
        page (PDPage. (apply rectangle (concat [0 0] mediabox)))
        opts {:page page}]
    (do
      (doto page
        (.setTrimBox (apply rectangle (concat [c c] [w h])))
        (.setBleedBox (apply rectangle (concat [(- c b) (- c b)] bleedbox))))
      (.addPage pdoc page)
      (engrave! opts elements)
      nil)))

(deftag content [pdoc page]
  (let [stream (PDPageContentStream. (@!context :pdoc) (@!context :page))
        opts   {:content-stream stream}]
    (do
      (engrave! opts elements)
      (.close stream)
      nil)))

#_(defn page
    "A PDPage."
    [{:keys [width height bleed trim]
      :or {width "210mm", height "297mm", bleed "5mm", trim "3mm"} :as options}
     & elements]
    (let [pdoc (@!context :pdoc)
          b (-> bleed m/toMeasurement m/toPoints :value)
          c (-> trim m/toMeasurement m/toPoints :value)
          w (-> width m/toMeasurement m/toPoints :value)
          h (-> height m/toMeasurement m/toPoints :value)
          mediabox (map (partial + (* 2 c)) [w h])
          bleedbox (map (partial + (* 2 b)) [w h])
          page (PDPage. (apply rectangle (concat [0 0] mediabox)))
          opts {:page page}]
      (do
        (doto page
          (.setTrimBox (apply rectangle (concat [c c] [w h])))
          (.setBleedBox (apply rectangle (concat [(- c b) (- c b)] bleedbox))))
        (.addPage pdoc page)
        (engrave! opts elements)
        nil)))

#_(defn content
    [{:keys [pdoc page] :as options}
     & elements]
    (let [stream (PDPageContentStream. (@!context :pdoc) (@!context :page))
          opts   {:content-stream stream}]
      (do
        (engrave! opts elements)
        (.close stream)
        nil)))
