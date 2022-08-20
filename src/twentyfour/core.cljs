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
;; Permutations are generated in the following form:
;;   [num1 num2 num3 num4] [op1 op2 op3] [form]
;; e.g.: 
;;   [1 2 8 20] [+ - /] [true]
;; When operated on, it would produce
;;   (/ 20 (- 8 (+ 2 1)))
;;
;; When the form is false, we use the alternative computation:
;;   [1 2 8 20] [+ - /] [false]
;; would produce
;;   (/ (- 20 8) (+ 2 1)))
(defn- generate-permutations [nums]
  (combo/cartesian-product 
    (combo/permutations nums)
    (combo/selections
     [;; :value indicates PEMDAS order
      ;; :reversible indicates if it's - or / (useful for PEMDAS notation)
      {:op + :display \+ :reverse false :reversible false :value 0}
      {:op - :display \- :reverse false :reversible true :value 0}
      {:op * :display \* :reverse false :reversible false :value 1}
      {:op / :display \/ :reverse false :reversible true :value 1}
      {:op div :display \/ :reverse true :reversible true :value 1}
      {:op sub :display \- :reverse true :reversible true :value 0}]
     (- (count nums) 1))
    [true false])
)

(defn- reduce24 [reduce-fn perm]
  (let [[nums ops form] (seq perm)]
      (reduce reduce-fn
            (first nums)
            (map vector (rest nums) ops))
      ))

(defn- alt-op [op num1 num2]
       ((op :op) num1 num2)
       )

(defn- solve-alternate [nums ops]
  (let [[num1 num2 num3 num4] (seq nums)
      [op1 op2 op3] (seq ops)]
    (alt-op op3 (alt-op op2 num4 num3) (alt-op op1 num2 num1))
  ))

(defn- operate [num tup]
  (let [[num2 op] tup]
    ((op :op) num2 num)))

(defn- solve24 [perm]
  (let [[nums ops form] (seq perm)]
    (if form 
      (reduce24 operate perm)
      (solve-alternate nums ops))))


(defn solve
  "Solves for 24 given a list of numbers in nums."
  [nums target]
  (filter (fn [perm]
    ;; Since ClojureScript does not support ratios, we check that
    ;; the result is within a small epsilon of the target.
            (< (abs (- target (solve24 perm))) 0.00001))
          (generate-permutations nums)))

(defn- pretty-print
  "Display expressions in human readable form (LISP notation)."
  [num tup]
  (let [[num2 op] tup
        display (op :display)]
    (if (op :reverse)
      (str \( display " " num " " num2 \))
      (str \( display " " num2 " " num \)))))

;; pretty-print only works for standard form for now
(defn- reduce-pretty-print [perm]
  (reduce24 pretty-print perm))

(defn- standard-print
  "Display expressions in human readable form (PEMDAS notation)."
  [state tup]
  (let [prev-value (state :value)
        [num2 op] tup
        curr-value (op :value)
        display (op :display)
        s (state :s)
        reverse (op :reverse)
        prev-state (if (and
                        ;; Add parentheses if the current op is reversible
                        ;; and the previous expression falls on the right
                        ;; e.g. 5 / (2 / 5)
                        (or (and (not reverse) (op :reversible))
                            ;; or if the PEMDAS order is greater now
                            ;; e.g. 4 * (5 + 2)
                            (< prev-value curr-value))
                        ;; Never add parentheses around the starting element
                        ;; 2 is always the starting value
                        (not= prev-value 2))
                        (str \( s \)) s)]
    (if reverse
      {:s (str prev-state " " display " " num2) :value curr-value}
      {:s (str num2 " " display " " prev-state) :value curr-value})))

(defn- reduce-standard-print [perm]
  (let [[nums ops form] (seq perm)]
    (:s
     (reduce standard-print
             ;; Set 2 to the starting value, since it should never
             ;; have a parentheses around it (and is greater than all
             ;; other possible values).
             {:s (str (first nums)) :value 2}
             (map vector (rest nums) ops)))))

(defn- standard-print-op-str [num1 display num2 add-parens]
       (if add-parens 
         (str \( num1 " " display " " num2 \))
         (str num1 " " display " " num2))
       )

(defn- standard-print-op
  [op num1 num2 add-parens]
  (let [display (op :display)]
    (if (op :reverse)
      (standard-print-op-str num2 display num1 add-parens)
      (standard-print-op-str num1 display num2 add-parens))))

;; TODO: fix this to remove unnecessary parentheses
(defn- standard-print-alternate [nums ops]
  (let [[num1 num2 num3 num4] (seq nums)
        [op1 op2 op3] (seq ops)]
          (str (standard-print-op op3 
                                  (standard-print-op op2 num4 num3 true)
                                  (standard-print-op op1 num2 num1 true) false))))

(defn- standard-print-forms [perm]
  (let [[nums ops form] (seq perm)]
    (if form
      (reduce-standard-print perm)
      (standard-print-alternate nums ops))))

;; HTML manipulation
(defn- set-value [id num]
  (dommy/set-value! (dommy/sel1 id) num))

(defn- get-url-int [name]
  (js/parseInt (.get url-params name)))

(defn- get-distinct-solutions []
  ;; Alternatively, we can use 'reduce-pretty-print' for LISP notation
  (distinct (map standard-print-forms
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

;; Can use (.log js/console ...) here to debug expressions