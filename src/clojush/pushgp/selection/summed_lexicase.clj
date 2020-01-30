(ns clojush.pushgp.selection.summed-lexicase
  (:use [clojush random]
        [clojush.pushgp.selection.lexicase]))

(defn sum-error-cases
  [individual case-group]
  (reduce +
          (map #(nth (:errors individual) % 0) case-group))
  )

(defn grouped-errors
  [cases individual]
  (map (partial sum-error-cases individual) cases))

(defn add-grouped-errors
  "Sums the errors in each partition of case numbers in cases, and assoc's
   that new error vector into each individual in the given population."
  [cases individual]
  (assoc individual :grouped-errors (grouped-errors cases individual))
  )

  (defn summed-partition-lexicase-selection-no-dropping
    "Selects an individual from the population using lexicase selection."
    [pop {summed-lexicase-factor :summed-lexicase-factor training-cases :training-cases :as argmap}]
    (let [partition-size (* summed-lexicase-factor (count training-cases))
          cases (partition partition-size partition-size nil (shuffle (range (count (:errors (first pop))))))]
      (lexicase-selection (map (partial add-grouped-errors cases) pop) argmap :grouped-errors)))
