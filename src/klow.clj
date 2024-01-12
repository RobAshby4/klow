(ns klow
  (:require [clojure.string :as str])
  (:require [clojure.tools.cli :refer [parse-opts cli]])
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:gen-class)
  )

(def max-ngrams 8)

(def cli-options 
  [ 
    ["-t" "--type TYPE" "Type of data input"
      :default "txt"
      :validate 
        [ #(case % 
             "txt" true 
             "twitter" true
             false)
         "Must be either txt or twitter"]]

    ["-i" "--input FILEPATH" "Input file for the algo"
      :default ""
      :validate
        [ #(.exists (io/as-file %)) "File not found"]]

    ["-h" "--help"]
  ])


(defn separate-full-text [tweet]
  (-> tweet
    (get "tweet")
    (get "full_text")))

(defn get-tweets [tweets]
  (map separate-full-text tweets))

(defn print-tweets [tweets]
  (doseq [tweet tweets]
  (println tweet)))

(defn check-if-retweet [tweet]
  (not (zero? (compare 
    (first (str/split tweet #" "))
    "RT"))))

(defn remove-retweets [tweets] 
  (filter 
    #(check-if-retweet %)
    tweets))

(defn remove-token-mention [tweet token]
  (apply str (interpose " "
    (filter
      #(not (str/includes? % token))
      (str/split tweet #" ")))))

(defn remove-links [tweets] 
  (map #(remove-token-mention % "http") tweets))

(defn remove-mentions [tweets] 
  (map #(remove-token-mention % "@") tweets))

(defn remove-empty [tweets] 
  (filter 
    #(> (count %) 0)
    tweets))

(defn get-tweets-from-file [filename]
  (-> filename
    (slurp)
    (json/read-str)
    (get "tweets")
    (get-tweets)))

(defn add-tokens-to-tweets [tweets]
  (map #(str "<s> " % " </s>") tweets))

(defn filter-data [tweets]
  (-> tweets
    (remove-retweets)
    (remove-links)
    (remove-mentions)
    (remove-empty)))

(defn parse-twitter-json [filepath]
  (-> filepath 
    (get-tweets-from-file)
    (filter-data)
    (add-tokens-to-tweets)))

(defn extract-ngrams [subset num-grams start-pos] 
  ;; recursively extract n-sized grams from 0..max-ngrams in len
    (if (and 
          (<= (+ start-pos num-grams) (count subset)) 
          (< num-grams max-ngrams)) 
      (apply conj 
             [(subvec subset start-pos (+ start-pos num-grams))]
             (extract-ngrams subset (inc num-grams) start-pos))
      nil))

(defn get-ngrams [subset]
  ;; sick oneliner
  (dedupe (map #(extract-ngrams subset 0 %) (range 0 (count subset))))
)

(defn build-ngrams [dataset] 
  (->> dataset
    (mapv #(str/split % #" "))
    ;; breaks each set of grams into n-gram from n = 0..max-ngrams 
    (mapv #(get-ngrams %))
    (apply concat)
    (mapv #(rest %))
    (reduce into [])
    ))

(defn get-possible [ngrams current]
  (filterv #(= (butlast %) current) ngrams))

(defn select-next [possible-grams]
  (let [next-gram 
      (nth 
        (shuffle possible-grams)
        (rand-int (count possible-grams)))]
    next-gram
  ))

(defn generate-output [ngrams current] 
  (if (= (last current) "</s>")
    (println "</s> \n end.")
    (let [possible-grams (get-possible ngrams current)]
      (if (zero? (count possible-grams))
        (generate-output ngrams (rest current))
        (let [next-gram (select-next possible-grams)]
          (print (str (last current) " "))
          (generate-output ngrams next-gram)
          )))))

(defn fetch-data [datatype filepath] 
  (case datatype
    :twt (parse-twitter-json filepath)
    :txt (str/split (slurp filepath) #"\n")))

(defn generator [dataset] 
  (-> dataset
    (build-ngrams)
    (generate-output ["<s>"])
  ))

(defn main [opts]
  (let [opt-map-options (parse-opts opts cli-options)]
     (println opt-map-options)
    )
  )
