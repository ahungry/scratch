(defn str
  "Join r.. strings into one."
  [& r]
  (string/join r))

(defn concat
  "Clojure like concat."
  [& r]
  (->> (reduce array/concat @[] r)
       (apply tuple)))
