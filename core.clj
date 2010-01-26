(ns peg-simple)

(defn make-terminal-parser [terminal]
  (fn [[_ input]]
    (if (= terminal (first input))
      (list true (next input))
      (list nil input))))

(defn peg-sequence [parse-expression1 parse-expression2 [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression1 data)]
    (if result
      (parse-expression2 return-data)
      (list nil input))))

(defn ordered-choice [parse-expression1 parse-expression2 [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression1 data)]
    (if result
      return-data
      (parse-expression2 data))))

(defn zero-or-more [parse-expression [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression data)]
    (if (not result)
      data
      (recur parse-expression return-data))))

(defn one-or-more [parse-expression [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression data)]
    (if result
      (zero-or-more parse-expression return-data)
      (list nil input))))

(defn optional [parse-expression [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression data)]
    (if result
      return-data
      data)))

(defn and-predicate? [parse-expression [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression data)]
    (if result
      data
      (list nil input))))

(defn not-predicate? [parse-expression [_ input :as data]]
  (let [[result remaining-input :as return-data] (parse-expression data)]
    (if result
      (list nil input)
      data)))

(defn test-stuff []
  (let [h (make-terminal-parser 'h)
	k (make-terminal-parser 'k)]
    (ordered-choice h (partial peg-sequence h k) '(true (h k a b c d)))))

(defn test-stuff2 []
  (let [h (make-terminal-parser 'h) ]
    (h (h '(true (h h a b c d))))))

(defn test-stuff3 []
  (let [h (make-terminal-parser 'h)
	k (make-terminal-parser 'k) ]
    (->> '(true (h h k a b c d)) (peg-sequence h h) k)))