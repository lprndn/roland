 ;;_Namespace

(ns roland.core
  (:import [org.apache.pdfbox.pdmodel PDDocument])
  (:require [roland.utils :refer [engrave! deftag context !context add-to-context! c>]]
            [roland.pdfobj :refer [page content]]
            [roland.colors :refer [ink]]
            [roland.font :refer [ttf]]
            [roland.papersizes]
            [roland.measurements]
            [roland.shapes :refer [fill-color s-color stroke-color rectangle circle line polygon]]
            [roland.typesetter :refer [viterbi lb1]]
            [roland.text :refer [justify]]
            [roland.layout :refer [layout debug-layout]]
            :reload-all))

;;;_* Roland Main Function

(defn roland
  "Takes a vector describing a pdf. Outputs a modified vector describing the final pdf. Produces the said pdf file as a side effect."
  [& elements]
  (do
    (if (map? (first elements))
      (engrave! (first elements) (next elements))
      (engrave! {} elements))
    @!context))
;;;_* Closure alternative

(deftag pdf [title location
             :or {title (or (@!context :title) "veillantif")
                  location "~/"}]
  (let [pdoc (PDDocument.)
        opts {:pdoc pdoc}]
    (engrave! opts elements)
    (doto pdoc
      (.save (str "/home/lprndn/" title ".pdf"))
      .close)))

;;;_* File type functions

#_(defn pdf
    "A PDDocument."
    [{:keys [title location]
      :or {title "veillantif", location "./"} :as options}
     & elements]
    (let [pdoc (PDDocument.)
          opts {:pdoc pdoc}]
      (engrave! opts elements)
      (doto pdoc
        (.save (str "/home/lprndn/" title ".pdf"))
        .close)
      nil))
;;;_* Archives

#_(defn roland
    "Takes a vector describing a pdf. Outputs a modified vector describing the final pdf. Produces the said pdf file as a side effect."
    [& elements]
    (do
      (if (map? (first elements))
        (engrave! (first elements) (drop 1 elements))
        (engrave! {} elements))
      @!context))


#_(defmacro roland
    [option elements]
    (if (not (empty? elements))
      (let [items (tree-seq #(vector? %) #(nthnext % 2) elements)
            elems (reverse items)]
        (reduce (fn [ac elem]
                  `(~(-> elem first name symbol)
                    *context* ~ac))
                '()
                elems))))

#_(defmacro rldold
    [options & elements]
    (if (not (empty? elements))
      (let [els (clojure.lang.RT/iter (reverse elements))]
        (loop [ac '()
               el (.next els)]
          (println el)
          (let [nac (conj ac `(binding [*context* (merge *context*
                                                         (~(-> el first name symbol) ~(second el))
                                                         #_(rld  (nthnext el 2)))]))]
            (if (.hasNext els)
              (recur nac
                     (.next els))
              nac))))))

#_(defmacro roland
    [elements]
    (if (not (empty? elements))
      (let [els (clojure.lang.RT/iter elements)]
        (loop [ac '()]))))

