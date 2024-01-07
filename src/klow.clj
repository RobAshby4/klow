(ns klow
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:require [clojure.math.combinatorics :as combo]))

;; (def input-filename "./shakespeare.txt")
(def input-filename "./src/test.txt")
(def tweet-file "./src/tweets.js")
(def max-ngrams 5)

(defn open-file [filename]
  (slurp filename))

(defn get-sample-size []
(-> input-filename
  (open-file)
  (str/split #"\n")
  (count)
  (println)))

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

(defn parse-twitter-json []
  (-> tweet-file
    (get-tweets-from-file)
    (filter-data)
    (add-tokens-to-tweets)))

;; dont ask me what happened here, it took a second to figure it out myself
(defn get-ngrams [subset]
  (if (seq subset)
    (reduce into [] 
      [
        (filterv 
          #(and 
             (< (count %) max-ngrams) 
             (> (count %) 0)) 
          (reductions conj [] subset))
        (get-ngrams (rest subset))
      ])
    nil))

(defn build-ngrams [dataset] 
  (->> dataset
    (map #(str/split % #" "))
    (map #(get-ngrams %))
    (apply concat)
    ))

(defn twt-main [] 
  (println "Training using twitter data")
  (->
    (parse-twitter-json)
    ;; (str/split (open-file input-filename) #"\n")
    (build-ngrams)
    (print-tweets)
  ))

(defn tmp-main []
    (println "which to parse? (twt txt)")
    (flush)
    (let [userin (read-line)]
    (cond 
        (= userin "txt") (println (open-file input-filename))
        (= userin "twt") (print-tweets (parse-twitter-json))
        :else (println "not supported"))))

(defn main [opts]
  (twt-main))
