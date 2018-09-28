(ns roland.text
  (:import [org.apache.pdfbox.pdmodel PDPageContentStream]
           [org.apache.pdfbox.pdmodel.font PDType1Font PDFont])
  (:require [roland.utils :refer [engrave! deftag !context]]
            [roland.font :refer [otf ttf]]
            [roland.typesetter :refer [break-paragraph]]))

#_(defn txt
    "Writes a string to a PDPageContentStream."
    [
     {:keys [content-stream font size color offsetx offsety leading word-spacing rendering matrix rotation scaling]
      :or {size 12 font (PDType1Font/COURIER) } :as options}
     text
     ]
    (.)
    (.showText content-stream text))


;;TODO Text manipulation should be a functions (closure?) to allow composition.
(deftag justify [text font size typesetter frame
                 :or {font (PDType1Font/COURIER)
                      size 12}]
  (let [lines (break-paragraph text font size typesetter)
        content-stream (@!context :content-stream)]
    (doto content-stream
      .beginText
      (.setFont font size)
      (.newLineAtOffset (float (:left frame)) (float (- (:top frame) size))))
    (doseq [line lines]
           (doto content-stream
             (.setWordSpacing (float (- (/ (- (:width frame) (reduce + (map :length line)))
                     (- (count line) 1))
                  (/ (* size (.getWidthFromFont font 32)) 1000))))
             (.showText (apply str (interleave (map :content line) (cycle " "))))
             (.newLineAtOffset (float 0) (float (- size)))))
    (.endText content-stream)))


#_(deftag write [pdoc content-stream font size color offsetx offsety
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



