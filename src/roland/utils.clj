(ns roland.utils)

(def !context (atom {}))

(defn add-to-context! [options]
  (swap! !context #(merge % options)))

(defprotocol Streamline
  (c! [x]))

(extend-protocol Streamline
  clojure.lang.Delay
  (c! [x] (force x))
  
  java.lang.Object
  (c! [x] x))

(defn force-eval-map [[key value]]
  [key (c! value)])

(defmacro deftag [name & definition]
  (let [[meta rest] (split-with #(not (vector? %)) definition)
        [arguments defaults] (split-with #(not (keyword? %)) (first rest))
        body (next rest)]
    `(defn ~name ~@meta [~(merge {:keys arguments} (apply hash-map defaults) {:as 'params})]
       (fn [& ~'elements]
         ~@body))))

(defn engrave!
  [options elements]
  (when (not (empty? options)) (add-to-context! options))
  (when (not (empty? elements))
    (doall (map (fn [el] (apply ((-> el first name symbol resolve)
                                (apply hash-map (mapcat force-eval-map (second el))))
                               (nnext el))) elements))
  #_(doseq [el elements]
      (let  [out (apply ((-> el first name symbol resolve) 
                         (apply hash-map (mapcat force-eval-map (second el)))) 
                        (nnext el))]
        (add-to-context! out)))))


(defn context
  [options & elements]
  (engrave! options elements))


(defn c> [path]
  (delay (path @!context)))

#_(defn old-engrave
    [options elements]
    (if (not (empty? elements))
      (reduce (fn [ac el] ;; opts: Map{:key value} el: Vector[:function {options} ...]
                (merge ac (apply (-> el first name symbol resolve)
                                 (conj (drop 2 el) (merge options ac (second el))))))
              {} elements)
      nil))

#_(defn engrave!
    [options elements]
    (if (not (empty? options)) (add-to-context! options))
    (if (not (empty? elements)) 
      (doseq [el elements]
        (let  [out (apply (-> el first name symbol resolve) (apply hash-map (mapcat force-eval-map (second el))) (drop 2 el))]
          (add-to-context! out)))))

#_(defn engrave!
    [options elements]
    (if (not (empty? elements))
      (reduce (fn [ac el]
                (merge ac (apply (-> el first name symbol resolve))
                       (conj (drop 2 el) (merge options ac (second el)))))
              {} elements)))


#_(defn engrave
    [options elements]
    (if (not (empty? elements))
      (reduce (fn [ac el]
                (binding 
                    [*context*  (merge options ac (second el))]
                  (merge ac (apply (-> el first name symbol resolve)
                                   (conj (drop 2 el) *context*))))))))

#_(defn engrave!
    [options elements]
    (if (not (empty? elements))
      (let [els (clojure.lang.RT/iter elements)]
        (loop [el (.next els)]
          (do 
            (add-to-context! (second el))
            (apply (-> el first name symbol resolve) (nthnext el 2))
            (if (.hasNext els)
              (recur (.next els))
              @!context))))
      @!context))


#_(defn engrave! 
    [options elements]
    (if (not (empty? elements))
      (let [els (clojure.lang.RT/iter (reverse elements))]
        (loop
            [ac '()
             el (.next els)]
          (if (.hasNext els) 
            (binding [*context* (merge *context* 
                                       (second el)
                                       (apply (-> el first name symbol)
                                              (nthnext el 2)))]))))
      nil))




#_(defn vengrave
    [options elements]
    (if (not (empty? elements))
      (doseq [e elements]
        (if (not (empty? e))
          (apply (-> e first name symbol resolve)
                 (conj (drop 2 e) (merge options (second e))))))))

#_(defn hengrave
    [options elements]
    (if (not (empty? elements))
      (reduce (fn [tp btm]
                (if (not (empty? btm))
                  (apply (-> btm first name symbol resolve)
                         (conj (drop 2 btm) (merge tp (second btm))))
                  options))
              options elements)))

#_(defn engrave
    "Translate a pdf definition into a serie of functions and execute it. Has side effects."
    [metadatas elements]
    (reduce (fn [opts  elem]
              (apply (-> elem first name symbol resolve)
                     (conj opts (second elem))
                     (drop 2 elem)))
            metadatas elements))
