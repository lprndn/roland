(ns roland.text
  (:import [org.apache.pdfbox.pdmodel PDPageContentStream]
           [org.apache.pdfbox.pdmodel.font PDType1Font PDFont])
  (:require [roland.utils :refer [engrave! deftag]]
            [roland.font :refer [otf ttf]]
            [roland.typesetter :refer [lb1]]))

#_(defn txt
    "Writes a string to a PDPageContentStream."
    [
     {:keys [content-stream font size color offsetx offsety leading word-spacing rendering matrix rotation scaling]
      :or {size 12 font (PDType1Font/COURIER) } :as options}
     text
     ]
    (.)
    (.showText content-stream text))

(deftag write [pdoc content-stream font size color offsetx offsety
               :or {font (PDType1Font/COURIER)
                    size 12
                    offsetx 100
                    offsety 100}]
  (doto content-stream
    .beginText
    (.newLineAtOffset offsetx offsety)
    (.setFont font size)
    (.showText (apply str elements))
    .endText)
  nil)

#_(defn write
    [{:keys [pdoc content-stream font size color offsetx offsety ]
      :or {font (PDType1Font/COURIER), size 12, offsetx 100, offsety 100} :as options}
     txt]
    (doto content-stream
      .beginText
      (.newLineAtOffset offsetx offsety)
      (.setFont font size)
      (.showText txt)
      .endText)
    nil)



