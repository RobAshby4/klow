(require '[clojure.string :as str])
(require '[clojure.data.json :as json])

;; (def input-filename "./shakespeare.txt")
(def input-filename "./test.txt")
(def tweet-file "./tweets.js")
(def max-ngrams 4)

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
    (print-tweets)))

(defn main []
  (doseq [arg *command-line-args*]
  (println (str "Read an argument: " arg))))

;; (main)
(parse-twitter-json)
