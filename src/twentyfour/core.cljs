(ns twentyfour.core
    (:require 
        [clojure.math.combinatorics :as combo]
        [clojure.string :as str]
        [dommy.core :as dommy]))

;; Set input values first to reduce delay for user.
(def url-params (new js/URLSearchParams js/window.location.search))

(defn- get-int [id]
  (js/parseInt (dommy/value (dommy/sel1 id))))

(defn- random-num []
  (inc (rand-int 12)))

(defn- set-url-params []
  (.set url-params "a" (get-int :#a))
  (.set url-params "b" (get-int :#b))
  (.set url-params "c" (get-int :#c))
  (.set url-params "d" (get-int :#d))
  (.set url-params "target" (get-int :#target))
  ;; Clear the answer field whenever updating URL params.
  (dommy/set-text! (dommy/sel1 :#answer) "")
  (.replaceState js/window.history (js-obj) ""
                 (str
                  ;; Preserve the URL before the query string to ensure
                  ;; it works when hosted on GitHub Pages as well.
                  (first (.split js/window.location.href "?"))
                  "?"
                  url-params)))

(defn- set-input-values []
  (let [a (.get url-params "a")
        b (.get url-params "b")
        c (.get url-params "c")
        d (.get url-params "d")
        all-nil (every? nil? [a b c d])]
    (dommy/set-value! (dommy/sel1 :#a) (if all-nil (random-num) a))
    (dommy/set-value! (dommy/sel1 :#b) (if all-nil (random-num) b))
    (dommy/set-value! (dommy/sel1 :#c) (if all-nil (random-num) c))
    (dommy/set-value! (dommy/sel1 :#d) (if all-nil (random-num) d)))
  (dommy/set-value! 
   (dommy/sel1 :#target) 
   (let [target (.get url-params "target")] (if (nil? target) "24" target)))
  (set-url-params))

(set-input-values)

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
(defn- set-value [id num]
  (dommy/set-value! (dommy/sel1 id) num))

(defn- get-url-int [name]
  (js/parseInt (.get url-params name)))

(defn- get-distinct-solutions []
  (distinct (map pretty-print-reduce
                 (solve [(get-url-int "a")
                         (get-url-int "b")
                         (get-url-int "c")
                         (get-url-int "d")]
                        (get-url-int "target")))))

(defn- solve-handler []
  (set-url-params)
  (dommy/set-text! (dommy/sel1 :#answer)
                   (let [solution (get-distinct-solutions)]
                     (if (empty? solution) 
                       "No solutions." 
                       (str/join "\n" solution)))))

(dommy/listen! (dommy/sel1 :#solve) :click solve-handler)

(defn- possible-handler []
  (set-url-params)
  (dommy/set-text! (dommy/sel1 :#answer)
                   (let [solution (get-distinct-solutions)]
                     (if (empty? solution) 
                       "Not possible." 
                       "Possible."))))

(dommy/listen! (dommy/sel1 :#possible) :click possible-handler)

(defn- random-handler []
  (set-value :#a (random-num))
  (set-value :#b (random-num))
  (set-value :#c (random-num))
  (set-value :#d (random-num))
  (set-url-params))

(dommy/listen! (dommy/sel1 :#random) :click random-handler)

;; Set up handlers to update URL params when any textbox changes.
(dommy/listen! (dommy/sel1 :#a) :change set-url-params)
(dommy/listen! (dommy/sel1 :#b) :change set-url-params)
(dommy/listen! (dommy/sel1 :#c) :change set-url-params)
(dommy/listen! (dommy/sel1 :#d) :change set-url-params)
(dommy/listen! (dommy/sel1 :#target) :change set-url-params)