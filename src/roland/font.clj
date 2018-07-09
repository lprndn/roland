;; * Namespace
(ns roland.font
  (:import
   [org.apache.fontbox.ttf OpenTypeFont TrueTypeFont TTFParser OTFParser]
   [org.apache.pdfbox.pdmodel.font PDFont PDTrueTypeFont PDType0Font]
   [org.apache.pdfbox.pdmodel.font.encoding StandardEncoding WinAnsiEncoding MacRomanEncoding MacExpertEncoding])
  (:require [roland.utils :refer [!context]]))

;; * Functions
(def encoding
  {:standard (StandardEncoding/INSTANCE)
   :mac-roman (MacRomanEncoding/INSTANCE)
   :mac-expert (MacExpertEncoding/INSTANCE)
   :win-ainsi (WinAnsiEncoding/INSTANCE)})

(defn ttf
  "Embed a ttf font in a pdfdocument."
  [{:keys [pdoc file encoding name] :as options}]
  (let [
        ttf (.parse (TTFParser. true) file)
        font (PDType0Font/load (@!context :pdoc) ttf false)
        opts (hash-map (keyword (or name (.getName font))) font)]
    opts))

(defn otf
  "Embed a otf font in a PDDocument."
  [{:keys [pdoc file encoding name] :as options}]
  (let [
        otf (.parse (OTFParser. true) file)
        font (PDType0Font/load (@!context :pdoc) otf false)
        opts (hash-map (keyword (or name (.getName font))) font)]
    opts))

