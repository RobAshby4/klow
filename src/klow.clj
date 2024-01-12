(ns klow
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json]))

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

(defn extract-ngrams [subset num-grams start-pos] 
  ;; recursively extract n-sized grams from 0..max-ngrams in len
    (if (and 
          (<= (+ start-pos num-grams) (count subset)) 
          (< num-grams max-ngrams)) 
      (apply conj 
             [(subvec subset start-pos (+ start-pos num-grams))]
             (extract-ngrams subset (inc num-grams) start-pos))
      nil 
  ))

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
        (int (rand (- (count possible-grams) 1))))]
    (println next-gram)
    next-gram
  ))

; TODO: Rewrite this. it doesnt work. its bad. it needs to go.
; (defn generate-output [ngrams current] 
;   (println (last current))
;   (if (empty? current) [] (
;     (let [possible-grams (get-possible ngrams current)]
;       (if (= (count possible-grams) 0)
;         (generate-output ngrams (rest current))
;         (let [next-gram (select-next possible-grams)]
;           (if (= (last next-gram) "</s>")
;             "</s>"
;             (conj 
;               (last [current])
;               (generate-output ngrams next-gram)))))))))

(defn twt-main [] 
  (println "Training using twitter data")
  (->
    (parse-twitter-json)
    (build-ngrams)
    ;; (generate-output ["<s>"])
    ;; now we have all n-grams
    ;; build up to max-ngram, decrement n if no match until </s>
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
