(ns twentyfour.core
    (:require 
        [clojure.math.combinatorics :as combo]
        [clojure.string :as str]
        [dommy.core :as dommy]))

(enable-console-print!)

;; 24 solving algorithm
(defn- abs [n] (max n (- n)))

(defn- div [a b] (/ b a))
(defn- sub [a b] (- b a))
(defn- generate-permutations [nums]
  (combo/cartesian-product 
    (combo/permutations nums)
    (combo/selections [
      {:op + :display \+ :reverse false}
      {:op - :display \- :reverse false}
      {:op * :display \* :reverse false}
      {:op / :display \/ :reverse false}
      {:op div :display \/ :reverse true}
      {:op sub :display \- :reverse true}
    ] (- (count nums) 1)))
)

(defn- operate [num tup]
  (((last tup) :op) (first tup) num)
)
(defn- operate-reduce [perm]
  (reduce operate
    (first (first (seq perm)))
    (map vector (rest (first (seq perm))) (last (seq perm))))
)

(defn solve
  "Solves for 24 given a list of numbers in nums."
  [nums target]
  (filter (fn [perm]
    ;; Since ClojureScript does not support ratios, we check that
    ;; the result is within a small epsilon of the target.
            (< (abs (- target (operate-reduce perm))) 0.00001))
          (generate-permutations nums)))

(defn- pretty-print 
  "Display expressions in human readable form."
  [num tup]
  (if ((last tup) :reverse)
    (str \( ((last tup) :display) " " num " " (first tup) \))
    (str \( ((last tup) :display) " " (first tup) " " num \))
    )
  )
(defn- pretty-print-reduce [perm]
  (reduce pretty-print
    (first (first (seq perm)))
    (map vector (rest (first (seq perm))) (last (seq perm))))
)

;; HTML manipulation
(defn- get-int [id] 
  (js/parseInt (dommy/value (dommy/sel1 id)))
 )

(defn- solve-handler []
  (dommy/set-text! (dommy/sel1 :#answer) 
    (str/join "\n" 
      (distinct (map pretty-print-reduce 
                     (solve [(get-int :#a)
                             (get-int :#b)
                             (get-int :#c)
                             (get-int :#d)] 24))))))

(dommy/listen! (dommy/sel1 :#solve) :click solve-handler)

(defn- possible-handler [_]
  (dommy/set-text! (dommy/sel1 :#answer)
                   (let [solution (solve [(get-int :#a)
                                          (get-int :#b)
                                          (get-int :#c)
                                          (get-int :#d)] 24)]
                     (if (empty? solution) 
                       "Not possible." 
                       "Possible."))))

(dommy/listen! (dommy/sel1 :#possible) :click possible-handler)