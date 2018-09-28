(def project 'roland)
(def version "0.1.0-SNAPSHOT")
(set-env! :resource-paths #{#_"resources" "src"}
          :repositories '[["central" "https://repo1.maven.org/maven2/"]
                          ["clojars" "https://clojars.org/repo"]
                          ["jitpack" "https://jitpack.io"]]
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [criterium "0.4.4"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [org.apache.pdfbox/pdfbox "RELEASE"]
                            [org.apache.pdfbox/fontbox "RELEASE"]
                            [org.apache.pdfbox/preflight "RELEASE"]
                            [com.github.alexbirkett/kiwi-java "1984349661"]
                            [sooheon/boot-gorilla "0.1.1-SNAPSHOT"]])

(task-options!
 aot {:namespace   #{'roland.core}}
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/roland"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main        'roland.core
      :file        (str "roland-" version "-standalone.jar")})

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[roland.core :as app])
  (apply (resolve 'app/-main) args))

(require '[adzerk.boot-test :refer [test]])
(require '[sooheon.boot-gorilla :refer [gorilla]])
