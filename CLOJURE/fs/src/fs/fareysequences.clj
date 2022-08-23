;; Andrey O. Matveev
;; After several selected formulas from the monograph A.O. Matveev, Farey Sequences: Duality and Maps Between Subsequences,
;; Berlin: De Gruyter, 2017, https://doi.org/10.1515/9783110547665 .
;;
;; Our Dramatis Personae (see Table 1.1 of the monograph):
;;
;; (Standard) Farey Sequence Fm ( in LaTeX: $\mathcal{F}_m$ ), where m > 0;
;; Farey Subsequence Fml ( in LaTeX: $\mathcal{F}_m^l$ ), where m > 1, and 0 < l < m;
;; Farey Subsequence Gml ( in LaTeX: $\mathcal{G}_m^l$ ), where m > 1, and 0 < l < m;
;; Farey Subsequence FB2mm ( in LaTeX: $\mathcal{F}(\mathbb{B}(2m),m)$ ), where m > 0;
;; Farey Subsequence FBnm ( in LaTeX: $\mathcal{F}(\mathbb{B}(n),m)$ ), where n > 1, and 0 < m < n.
;;
;; In healthy situations, all of the exported functions below return reduced fractions (h/k) such that (0/1) <= (h/k) <= (1/1).
;; If you get a NEGATIVE fraction, it means that something went wrong,
;; and the denominator of the resulting negative fraction has no computational meaning,
;; since it just reports a reason of the problem---see the source code of the function you have used.
;;
;; If you would like to build up a certain subsequence of neighboring (one after another) fractions
;; in one of the above Personae, then first you should realize what you hold in your hands in the beginning.
;;
;; If you have two neighboring fractions, then you proceed in the recurrent manner
;; by means of a relatively fast function of the form `predecessor-of-pair-of-neighbors-in-personage' or
;; `successor-of-pair-of-neighbors-in-personage', depending on which direction (descending or ascending) you take.
;;
;; If you have one fraction then, in order to obtain your starting pair of neighboring fractions,
;; you make one-time use of a relatively slow function of the form `predecessor-in-personage' or `successor-in-personage',
;; and then you proceed by calling, in the recurrent manner, the corresponding relatively fast function
;; of the form  `predecessor-of-pair-of-neighbors-in-personage' or
;; `successor-of-pair-of-neighbors-in-personage'.
;;
;;
;; In theory, Farey Subsequences $\mathcal{F}(\mathbb{B}(n),m)$ exhibit different behavior 
;; depending on whether you have (== n (* 2 m)), or (not= n (* 2 m)). Nevertheless, if you deal with a Farey Subsequence 
;; $\mathcal{F}(\mathbb{B}(2m),m)$, then in this namespace it is suffices to call the predecessor-in-FBnm, successor-in-FBnm, 
;; predecessor-of-pair-of-neighbors-in-FBnm, and successor-of-pair-of-neighbors-in-FBnm functions by giving the value of (* 2 m) for the parameter n.
;;
;; If, for some reason, you prefer more narrowed names of functions, just make those functions public, by replacing `defn-' with `defn'.

(ns fs.fareysequences)

(defn- predecessor-of-one-first-in-Fm [m]
;; See Remark 1.6 and Table 1.5  of the monograph   
  (/ (dec m) m))

(defn- successor-of-zero-first-in-Fm [m]
;; See Remark 1.6 and Table 1.5  of the monograph
  (/ 1 m))

(defn- predecessor-of-one-first-in-Fml [l]
;; See Remark 1.9 and Table 1.5  of the monograph
  (/ l (inc' l)))

(defn- successor-of-zero-first-in-Fml [m]
;; See Remark 1.9 and Table 1.5  of the monograph
  (/ 1 m))

(defn- predecessor-of-one-first-in-Gml [m]
;; See Remark 1.13 and Table 1.5  of the monograph
  (/ (dec m) m))

(defn- successor-of-zero-first-in-Gml [m l]
;; See Remark 1.13 and Table 1.5  of the monograph
  (/ 1 (inc' (- m l))))

(defn- find-numerator-of-predecessor [searchInterval successor]
  (some  #(if (== (mod (inc' (*' (denominator successor) %)) (numerator successor)) 0)  %)
         (range (first searchInterval) (inc' (second searchInterval)))))

(defn- find-numerator-of-successor [searchInterval predecessor]
  (some  #(if (== (mod (dec (*' (denominator predecessor) %)) (numerator predecessor)) 0)  %)
         (range (first searchInterval) (inc' (second searchInterval)))))

(defn- get-numerator-and-return-predecessor [a successor]
  (clojure.lang.Numbers/toRatio
   (/ a (quot (inc' (*' (denominator successor) a)) (numerator successor)))))

(defn- get-numerator-and-return-successor [a predecessor]
  (clojure.lang.Numbers/toRatio
   (/ a (quot (dec (*' (denominator predecessor) a)) (numerator predecessor)))))

(defn- predecessor-of-one-first-in-FB2mm [m]
;; See Remark 1.17 and Table 1.5 of the monograph
  (/ m (inc' m)))

(defn- predecessor-of-two-thirds-in-FB2mm [m]
;; See Remark 2.25 and Table 2.8 of the monograph
  (if (even? m)
    (/ (dec m) (quot  (- (* 3 m) 2) 2))
    (/ m (quot (inc' (*' 3 m)) 2))))

(defn- predecessor-of-one-second-in-FB2mm [m]
;; See Remark 2.11 and Table 2.5 of the monograph
  (/ (dec m) (dec (*' 2 m))))

(defn- predecessor-of-one-third-in-FB2mm [m]
;; See Remark 2.24 and Table 2.7 of the monograph
  (if (even? m)
    (/ (quot (- m 2) 2) (quot  (- (* 3 m) 4) 2))
    (/ (quot (- m 1) 2) (quot (dec (*' 3 m)) 2))))

(defn- successor-of-zero-first-in-FB2mm [m]
;; See Remark 1.17 and Table 1.5 of the monograph
  (/ 1 (inc m)))

(defn- successor-of-one-third-in-FB2mm [m]
;; See Remark 2.24 and Table 2.7 of the monograph
  (if (even? m)
    (/ (quot m 2) (quot  (- (* 3 m) 2) 2))
    (/ (quot (inc' m) 2) (quot (inc' (*' 3 m)) 2))))

(defn- successor-of-one-second-in-FB2mm [m]
;; See Remark 2.11 and Table 2.5 of the monograph
  (/ m (dec (* 2 m))))

(defn- successor-of-two-thirds-in-FB2mm [m]
;; See Remark 2.25 and Table 2.8 of the monograph
  (if (even? m)
    (/ (dec m) (quot  (- (* 3 m) 4) 2))
    (/ m (quot (dec (*' 3 m)) 2))))

(defn- predecessor-of-one-first-in-FBnm [m]
;; See Remark 1.17 and Table 1.5 of the monograph
  (/ m (inc' m)))

(defn- predecessor-of-two-thirds-in-FBnm [n m]
;; See CORRECTED Remark 2.43(i) and Remark 2.43(ii) and CORRECTED Table 2.8 of the monograph
  (cond
    (< n (*' 2 m))  (if (>= (- (*' 2 n) (*' 3 m)) 1)
                      (if (even? m)
                        (/ (dec m) (quot (- (*' 3 m) 2) 2))
                        (/ m (quot (+' (*' 3 m) 1) 2)))
                      (/ (dec (*' (- n m) 2)) (dec (*' (- n m) 3))))
    (even? m)  (/ (dec m) (quot (- (*' 3 m) 2) 2))
    :else (/ m (quot (+' (*' 3 m) 1) 2))))

(defn- predecessor-of-one-second-in-FBnm [n m]
;; See Remark 2.17 and Table 2.5 of the monograph
  (if (< n (*' 2 m))
    (/ (dec (- n m))  (dec (*' (- n m) 2)))
    (/ m (inc' (*' 2 m)))))

(defn- predecessor-of-one-third-in-FBnm [n m]
;; See CORRECTED Remark 2.42 and Table 2.7 of the monograph
  (cond
    (< n (*' 2 m)) (if (even? (- n m))
                     (/ (quot (- (- n m) 2) 2) (quot (- (*' 3 (- n m)) 4) 2))
                     (/ (quot (dec (- n m)) 2) (quot (dec  (*' 3 (- n m))) 2)))
    (>= (- n (*' 3 m)) 1) (/ m (inc' (*' 3 m)))
    (even? (- n m)) (/ (quot (- (- n m) 2) 2) (quot (- (*' 3 (- n m)) 4) 2))
    :else (/ (quot (dec (- n m)) 2) (quot  (dec (*' 3 (- n m))) 2))))

(defn- successor-of-zero-first-in-FBnm [n m]
;; See Remark 1.17 and Table 1.5 of the monograph
  (/ 1 (inc' (- n m))))

(defn- successor-of-one-third-in-FBnm [n m]
;; See CORRECTED Remark 2.42 and Table 2.7 of the monograph
  (cond
    (< n (*' 2 m)) (if (even? (- n m))
                     (/ (quot (- n m) 2) (quot (- (*' 3 (- n m)) 2) 2))
                     (/ (quot (inc' (- n m)) 2) (quot (inc' (*' 3 (- n m))) 2)))
    (<= (- (*' 3 m) n) 1) (/ m (dec (*' 3 m)))
    (even? (- n m)) (/ (quot (- n m) 2) (quot (- (*' 3 (- n m)) 2) 2))
    :else (/ (quot (inc' (- n m)) 2) (quot  (inc' (*' 3 (- n m))) 2))))

(defn- successor-of-one-second-in-FBnm [n m]
;; See Remark 2.17 and Table 2.5 of the monograph
  (if (< n (*' 2 m))
    (/ (inc' (- n m))  (inc' (*' (- n m) 2)))
    (/ m (dec (*' 2 m)))))

(defn- successor-of-two-thirds-in-FBnm [n m]
;; See Remarks 2.43(i)-(ii) and Table 2.8 of the monograph
  (cond
    (< n (*' 2 m)) (if (>= (- (*' 3 m) (*' 2 n)) 1)
                     (/ (inc' (*' (- n m) 2)) (inc' (*' (- n m) 3)))
                     (if (even? m) (/ (dec m) (quot (- (* 3 m) 4) 2))
                         (/ m (quot (dec (* 3 m)) 2))))
    (even? m) (/ (dec m) (quot (- (* 3 m) 4) 2))
    :else (/ m (quot (dec (*' 3 m)) 2))))

(defn predecessor-in-Fm [m ^clojure.lang.Ratio successor]
;; See Lemma 2.9(i) and Table 2.1 of the monograph. Call for instance: 
;;    user>   (predecessor-in-Fm 6 (/ 2 3)) 
;; to get the result: 
;;    3/5
;; Call: 
;;    user>   (predecessor-in-Fm 6 (clojure.lang.Numbers/toRatio (/ 1 1))) 
;; to get the result: 
;;    5/6 
;; Call: 
;;    user>   (predecessor-in-Fm 6 (clojure.lang.Numbers/toRatio 1)) 
;; to get the same result: 
;;    5/6
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (not= (type successor) clojure.lang.Ratio) (/ 1 (- 2))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    (< m 1) (/ 1 (- 3))   ;"N/A: Order m of the sequence should be > 0"    
    (or (<= successor (/ 0 1)) (> successor (/ 1 1))) (/ 1 (- 4))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
    (> (denominator successor) m) (/ 1 (- 5))   ;"N/A: Denominator of the successor should not exceed the order m of the sequence" 
    :else
    (if (== successor (/ 1 1)) (predecessor-of-one-first-in-Fm m)
        (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) m) (denominator successor))))]
          (get-numerator-and-return-predecessor 
           (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor)))))

(defn successor-in-Fm [m ^clojure.lang.Ratio predecessor]
;; See Lemma 2.9(ii) and Table 2.3 of the monograph. Call for instance:
;;    user>   (successor-in-Fm 6 (/ 1 3)) 
;; to get the result: 
;;    2/5
;; Call: 
;;    user>   (successor-in-Fm 6 (clojure.lang.Numbers/toRatio (/ 0 1))) 
;; to get the result: 
;;    1/6 
;; Call: 
;;    user>   (successor-in-Fm 6 (clojure.lang.Numbers/toRatio 0)) 
;; to get the same result: 
;;    1/6
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 2))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    (< m 1) (/ (- 3))   ;"N/A: Order m of the sequence should be > 0"    
    (or (< predecessor (/ 0 1)) (>= predecessor (/ 1 1))) (/ 1 (- 4))   ;"N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
    (> (denominator predecessor) m) (/ 1 (- 5))   ;"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence" 
    :else
    (if (== predecessor (/ 0 1)) (successor-of-zero-first-in-Fm m)
        (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) m) 2) (denominator predecessor))))]
          (get-numerator-and-return-successor 
           (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor)))))

(defn predecessor-in-Fml [m l ^clojure.lang.Ratio successor]
;; See Lemma 2.13(i)(a)-(b) and Table 2.1 of the monograph. Call for instance:
;;    user>   (predecessor-in-Fml 6 4 (/ 3 5)) 
;; to get the result: 
;;    1/2
;; Call: 
;;    user>   (predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) 
;; to get the result: 
;;    4/5 
;; Call: 
;;    user>   (predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio 1)) 
;; to get the same result: 
;;    4/5
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    (not= (type successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    (< m 2) (/ 1 (- 4))   ;"N/A: Order m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 5))  ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"    
    (or (<= successor (/ 0 1)) (> successor (/ 1 1))) (/ 1 (- 6))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
    (> (denominator successor) m) (/ 1 (- 7))   ;"N/A: Denominator of the successor should not exceed the parameter m of the sequence" 
    (< l (numerator successor))  (/ 1 (- 8))   ;"N/A: Numerator of the successor should be between 1 (included) and l (included)"
    :else
    (if (== successor (/ 1 1)) (predecessor-of-one-first-in-Fml l)
        (if (>=  (- (*' (numerator successor) m) (*' (denominator successor) l)) 1)
          (get-numerator-and-return-predecessor
           (find-numerator-of-predecessor [(inc' (- l (numerator successor))) l] successor) successor)
          (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) m) (denominator successor))))]
            (get-numerator-and-return-predecessor
             (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor))))))

(defn successor-in-Fml [m l ^clojure.lang.Ratio predecessor]
;; See Lemma 2.13(ii)(a)-(b) and Table 2.3 of the monograph. Call for instance:
;;    user>   (successor-in-Fml 6 4 (/ 4 5)) 
;; to get the result: 
;;    3/4
;; Call: 
;;    user>   (successor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 0 1))) 
;; to get the result: 
;;    1/6 
;; Call: 
;;    user>   (successor-in-Fml 6 4 (clojure.lang.Numbers/toRatio 0)) 
;; to get the same result: 
;;    1/6
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    (not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    (< m 2) (/ 1 (- 4))   ;"N/A: Order m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 5))  ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"    
    (or (< predecessor (/ 0 1)) (>= predecessor (/ 1 1))) (/ 1 (- 6))   ;"N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
    (> (denominator predecessor) m) (/ 1 (- 7))   ;"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence" 
    (< l (numerator predecessor))  (/ 1 (- 8))   ;"N/A: Numerator of the predecessor should be between 1 (included) and l (included)"
    :else
    (if (== predecessor (/ 0 1))
      (successor-of-zero-first-in-Fml m)
      (if (>= (- (*' (denominator predecessor)  l) (*' (numerator predecessor) m)) 1)
        (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) m) 2) (denominator predecessor))))]
          (get-numerator-and-return-successor
           (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor))
        (get-numerator-and-return-successor
         (find-numerator-of-successor [(inc' (- l (numerator predecessor))) l] predecessor) predecessor)))))

(defn predecessor-in-Gml [m l ^clojure.lang.Ratio successor]
;; See Lemma 2.15(i)(a)-(b) and Table 2.1 of the monograph. Call for instance:
;;    user>   (predecessor-in-Gml 6 4 (/ 1 3)) 
;; to get the result: 
;;    0/1
;; Call: 
;;    user>   (predecessor-in-Gml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) 
;; to get the result: 
;;    5/6
;; Call: 
;;    user>   (predecessor-in-Gml 6 4 (clojure.lang.Numbers/toRatio 1)) 
;; to get the same result: 
;;    5/6
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    (not= (type successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    (< m 2) (/ 1 (- 4))   ;"N/A: Order m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 5))  ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"    
    (or (<= successor (/ 0 1)) (> successor (/ 1 1))) (/ 1 (- 6))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
    (> (denominator successor) m) (/ 1 (- 7))   ;"N/A: Denominator of the successor should not exceed the parameter m of the sequence" 
    (> (- (+' l (numerator successor)) m) (numerator successor)) (/ 1 (- 8))   ;"N/A: The quantity (l + denominator - m) should not exceed the numerator of the successor"
    :else
    (if (== successor (/ 1 1))
      (predecessor-of-one-first-in-Gml m)
      (if (>= (- (*' (numerator successor)  m) (*' (denominator successor) l)) 1)
        (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) m) (denominator successor))))]
          (get-numerator-and-return-predecessor
           (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor))
        (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) (- m l)) (- (denominator successor) (numerator successor)))))]
          (get-numerator-and-return-predecessor
           (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor))))))

(defn successor-in-Gml [m l ^clojure.lang.Ratio predecessor]
;; See Lemma 2.15(ii)(a)-(b) and Table 2.3 of the monograph. Call for instance:
;;    user>   (successor-in-Gml 6 4 (/ 1 3)) 
;; to get the result: 
;;    1/2
;; Call: 
;;    user>   (successor-in-Gml 6 4 (clojure.lang.Numbers/toRatio (/ 0 1))) 
;; to get the result: 
;;    1/3
;; Call: 
;;    user>   (successor-in-Gml 6 4 (clojure.lang.Numbers/toRatio 0)) 
;; to get the same result: 
;;    1/3
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    (not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    (< m 2) (/ 1 (- 4))   ;"N/A: Order m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 5))  ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"    
    (or (< predecessor (/ 0 1)) (>= predecessor (/ 1 1))) (/ 1 (- 6))   ;"N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
    (> (denominator predecessor) m) (/ 1 (- 7))   ;"N/A: Denominator of the successor should not exceed the parameter m of the sequence" 
    (> (- (+' l (denominator predecessor)) m) (numerator predecessor)) (/ 1 (- 8))   ;"N/A: Denominator of the predecessor minus its numerator should not exceed (m - l)"
    :else
    (if (== predecessor (/ 0 1))
      (successor-of-zero-first-in-Gml m l)
      (if (>= (- (*' (denominator predecessor)  l) (*' (numerator predecessor) m)) 1)
        (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) (- m l)) 2) (- (denominator predecessor) (numerator predecessor)))))]
          (get-numerator-and-return-successor
           (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor))
        (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) m) 2) (denominator predecessor))))]
          (get-numerator-and-return-successor
           (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor))))))

(defn- predecessor-in-FB2mm [m ^clojure.lang.Ratio successor]
;; See Remark 1.17 and Table 1.5;
;; see Remark 2.25 and Table 2.8;
;; see Remark 2.11 and Table 2.5;
;; see Remark 2.24 and Table 2.7;
;; see Proposition 2.12 (i) (a) and Proposition 2.12 (ii) (a), and Table 2.1 of the monograph. Call for instance:
;;    user> (predecessor-in-FB2mm 3 (/ 2 5))
;;- to get the result:
;;    1/3
;; Call: 
;;    user>   (predecessor-in-FB2mm 3 (clojure.lang.Numbers/toRatio (/ 1 1))) 
;; to get the result: 
;;    3/4
;; Call: 
;;    user>   (predecessor-in-FB2mm 3 (clojure.lang.Numbers/toRatio 1)) 
;; to get the same result: 
;;    3/4
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (not= (type successor) clojure.lang.Ratio) (/ 1 (- 2))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    (< m 1) (/ 1 (- 3))   ;"N/A: Parameter m of the sequence should be > 0"
    (or (<= successor (/ 0 1)) (> successor (/ 1 1))) (/ 1 (- 4))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
    (> (denominator successor) (* 2 m)) (/ 1 (- 5))   ;"N/A: Denominator of the successor should not exceed  (2 * m)"
    (or (> (- (denominator successor) m) (numerator successor)) (> (numerator successor) m)) (/ 1 (- 6)) ;"N/A: Numerator of the successor should be between (denominator - m) (included) and m (included)"
    :else
    (cond
      (== successor (/ 1 1)) (predecessor-of-one-first-in-FB2mm m)
      (== successor (/ 2 3)) (predecessor-of-two-thirds-in-FB2mm m)
      (== successor (/ 1 2)) (predecessor-of-one-second-in-FB2mm m)
      (== successor (/ 1 3)) (predecessor-of-one-third-in-FB2mm m)
      :else (if (> successor (/ 1 2))
              (get-numerator-and-return-predecessor
               (find-numerator-of-predecessor [(inc' (- m (numerator successor))) m] successor) successor)
              (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) m) (- (denominator successor) (numerator successor)))))]
                (get-numerator-and-return-predecessor
                 (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor))))))

(defn- successor-in-FB2mm [m ^clojure.lang.Ratio predecessor]
;; See Remark 1.17 and Table 1.5;
;; see Remark 2.24 and Table 2.7;
;; see Remark 2.11 and Table 2.5;
;; see Remark 2.25 and Table 2.8;
;; see Proposition 2.12 (i) (b) and Proposition 2.12 (ii) (b), and Table 2.3 of the monograph. Call for instance:
;;    user>   (successorInFB2mm 3 (/ 3 5))
;; to get the result:
;;    2/3
;; Call: 
;;    user>   (successor-in-FB2mm 3 (clojure.lang.Numbers/toRatio (/ 0 1))) 
;; to get the result: 
;;    1/4
;; Call: 
;;    user>   (successor-in-FB2mm 3 (clojure.lang.Numbers/toRatio 0)) 
;; to get the same result: 
;;    1/4
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 2))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    (< m 1) (/ 1 (- 3))   ;"N/A: Parameter m of the sequence should be > 0"
    (or (< predecessor (/ 0 1)) (>= predecessor (/ 1 1))) (/ 1 (- 4))   ;"N/A: predecessor should be between (0/1) (excluded) and (1/1) (included)"
    (> (denominator predecessor) (* 2 m)) (/ 1 (- 5))   ;"N/A: Denominator of the predecessor should not exceed (2 * m)"
    (or (> (- (denominator predecessor) m) (numerator predecessor)) (> (numerator predecessor) m)) (/ 1 (- 6)) ;"N/A: Numerator of the predecessor should be between (denominator - m) (included) and m (included)"
    :else
    (cond
      (== predecessor (/ 0 1)) (successor-of-zero-first-in-FB2mm m)
      (== predecessor (/ 1 3)) (successor-of-one-third-in-FB2mm m)
      (== predecessor (/ 1 2)) (successor-of-one-second-in-FB2mm m)
      (== predecessor (/ 2 3)) (successor-of-two-thirds-in-FB2mm m)
      :else (if (< predecessor (/ 1 2))
              (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) m) 2) (- (denominator predecessor) (numerator predecessor)))))]
                (get-numerator-and-return-successor
                 (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor))
              (get-numerator-and-return-successor
               (find-numerator-of-successor [(inc' (- m (numerator predecessor))) m] predecessor) predecessor)))))

(defn predecessor-in-FBnm [n m ^clojure.lang.Ratio successor]
;; See Remark 1.17 and Table 1.5 of the monograph;
;; see CORRECTED Remark 2.43(i) and Remark 2.43(ii) and CORRECTED Table 2.8;
;; see Remark 2.17 and Table 2.5;
;; See CORRECTED Remark 2.42 and Table 2.7;
;; see Propositions 2.18(i)(a) and 2.18(ii)(a), and Table 2.1;
;; see Propositions 2.19(i)(a) and 2.19(ii)(a), and Table 2.1.  Call for instance:
;;    user>   (predecessor-in-FBnm 6 4 (/ 3 4))
;;- to get the result:
;;    2/3
;; Call: 
;;    user>   (predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) 
;; to get the result: 
;;    4/5
;; Call: 
;;    user>   (predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio 1)) 
;; to get the same result: 
;;    4/5
  (cond
    (and (not= (type n) java.lang.Long) (not= (type n) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ (- 1) 1))   ;"N/A: Type of n must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (not= (type successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of successor must be clojure.lang.Ratio" 		      
    (== n (*' 2 m)) (predecessor-in-FB2mm m successor)
    (< n 2) (/ 1 (- 4))    ;"N/A: Parameter n of the sequence should be > 1" 
    (or (< m 1) (>= m n)) (/ 1 (- 5))    ;"N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
    (or (<= successor (/ 0 1)) (> successor (/ 1 1))) (/ 1 (- 6))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
    (> (denominator successor) n) (/ 1 (- 7))   ;"N/A: Denominator of the successor should not exceed n"
    (or (> (- (+ m (denominator successor)) n) (numerator successor)) (> (numerator successor) m)) (/ 1 (- 8)) ;"N/A: Numerator of the successor should be between (m + denominator - n) (included) and m (included)"
    :else (cond
            (== successor (/ 1 1)) (predecessor-of-one-first-in-FBnm m)
            (== successor (/ 2 3)) (predecessor-of-two-thirds-in-FBnm n m)
            (== successor (/ 1 2)) (predecessor-of-one-second-in-FBnm n m)
            (== successor (/ 1 3)) (predecessor-of-one-third-in-FBnm n m)
            :else (if (< n (*' 2 m))
                    (if (and (> successor (/ 1 2)) (>= (- (*' (numerator successor) n) (*' (denominator successor) m)) 1))
                      (get-numerator-and-return-predecessor
                       (find-numerator-of-predecessor [(inc' (- m (numerator successor))) m] successor) successor)
                      (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) (- n m)) (- (denominator successor) (numerator successor)))))]
                        (get-numerator-and-return-predecessor
                         (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor)))
                    (if (or (> successor (/ 1 2)) (>= (- (*' (numerator successor) n) (*' (denominator successor) m)) 1))
                      (get-numerator-and-return-predecessor
                       (find-numerator-of-predecessor [(inc' (- m (numerator successor))) m] successor) successor)
                      (let [ref-point (bigint (Math/ceil (/ (*' (numerator successor) (- n m)) (- (denominator successor) (numerator successor)))))]
                        (get-numerator-and-return-predecessor
                         (find-numerator-of-predecessor [(- ref-point (numerator successor)) (dec ref-point)] successor) successor)))))))

(defn successor-in-FBnm [n m ^clojure.lang.Ratio predecessor]
;; See Remark 1.17 and Table 1.5 of the monograph;
;; see CORRECTED Remark 2.42 and Table 2.7;
;; see Remark 2.17 and Table 2.5;
;; see Remark 2.43(i)-(ii) and Table 2.8;
;; see Propositions 2.18(i)(b) and 2.18(ii)(b), and Table 2.3;
;; see Propositions 2.19(i)(b) and 2.19(ii)(b), and Table 2.3.  Call for instance:
;;    user>   (successor-in-FBnm 6 4 (/ 4 5))
;; to get the result:
;;    1/1
;; Call: 
;;    user>   (successor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 0 1))) 
;; to get the result: 
;;    1/3
;; Call: 
;;    user>   (successor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio 0)) 
;; to get the same result: 
;;    1/3
  (cond
    (and (not= (type n) java.lang.Long) (not= (type n) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ (- 1) 1))   ;"N/A: Type of n must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of predecessor must be clojure.lang.Ratio" 		      
    (== n (*' 2 m)) (successor-in-FB2mm m predecessor)
    (< n 2) (/ 1 (- 4))   ;"N/A: Parameter n of the sequence should be > 1" 
    (or (< m 1) (>= m n)) (/ 1 (- 5))   ;"N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
    (or (< predecessor (/ 0 1)) (>= predecessor (/ 1 1))) (/ 1 (- 6))   ;"N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
    (> (denominator predecessor) n) (/ 1 (- 7))   ;"N/A: Denominator of the predecessor should not exceed n"
    (or (> (- (+ m (denominator predecessor)) n) (numerator predecessor)) (> (numerator predecessor) m)) (/ 1 (- 8)) ;"N/A: Numerator of the predecessor should be between (m + denominator - n) (included) and m (included)"
    :else (cond
            (== predecessor (/ 0 1)) (successor-of-zero-first-in-FBnm n m)
            (== predecessor (/ 1 3)) (successor-of-one-third-in-FBnm n m)
            (== predecessor (/ 1 2)) (successor-of-one-second-in-FBnm n m)
            (== predecessor (/ 2 3)) (successor-of-two-thirds-in-FBnm n m)
            :else (if (< n (*' 2 m))
                    (if (and (> predecessor (/ 1 2)) (<= (- (*' (denominator predecessor) m) (*' (numerator predecessor) n)) 1))
                      (get-numerator-and-return-successor
                       (find-numerator-of-successor [(inc' (- m (numerator predecessor))) m] predecessor) predecessor)
                      (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) (- n m)) 2) (- (denominator predecessor) (numerator predecessor)))))]
                        (get-numerator-and-return-successor
                         (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor)))
                    (if (or (> predecessor (/ 1 2)) (<= (- (*' (denominator predecessor) m) (*' (numerator predecessor) n)) 1))
                      (get-numerator-and-return-successor
                       (find-numerator-of-successor [(inc' (- m (numerator predecessor))) m] predecessor) predecessor)
                      (let [ref-point (bigint (Math/ceil (/ (+' (*' (numerator predecessor) (- n m)) 2) (- (denominator predecessor) (numerator predecessor)))))]
                        (get-numerator-and-return-successor
                         (find-numerator-of-successor [(- ref-point (numerator predecessor)) (dec ref-point)] predecessor) predecessor)))))))

(defn predecessor-of-pair-of-neighbors-in-Fm [m [^clojure.lang.Ratio successor ^clojure.lang.Ratio right-neighbor-of-successor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fm
;; See Proposition 1.25 and Table 1.6 of the monograph. Call for instance:
;;    user>   (predecessor-of-pair-of-neighbors-in-Fm 6 [(/ 1 3) (/ 2 5)] true)
;; to get the result:
;;    1/4
;; Also call:
;;    user>   (predecessor-of-pair-of-neighbors-in-Fm 6 [(predecessor-in-Fm 6 (/ 2 5)) (/ 2 5)] false)
;; to get the same result:
;;    1/4
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type successor) clojure.lang.Ratio) (/ 1 (- 2))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    ;(not= (type right-neighbor-of-successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of right-neighbor-of-successor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 4))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< m 1) (/ 1 (- 5))   ;"N/A: Order m of the sequence should be > 0"
    (>= successor right-neighbor-of-successor) (/ 1 (- 6))   ;"N/A: We should have successor < right-neighbor-of-successor" 
    (or (<= successor (/ 0 1)) (>= successor (/ 1 1))) (/ 1 (- 7))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (excluded)" 
    (> (denominator successor) m)  (/ 1 (- 8))  ;"N/A: Denominator of the successor should not exceed the order m of the sequence"    
    (> right-neighbor-of-successor (/ 1 1)) (/ 1 (- 9))   ;"N/A: right-neighbor-of-successor should be between successor (excluded) and (1/1) (included)"
    (> (denominator right-neighbor-of-successor) m)  (/ 1 (- 10))   ;"N/A: Denominator of the rightNeighborOfSuccessor should not exceed the order m of the sequence"
    (or (not check-pair) (and check-pair (== successor (predecessor-in-Fm m right-neighbor-of-successor))))
    (let [farey-index (bigint (Math/floor (/ (+ m (denominator right-neighbor-of-successor))
                                             (denominator successor))))]
      (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                       (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor)))))
    :else (/ 1 (- 11))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey sequence"
    ))

(defn successor-of-pair-of-neighbors-in-Fm [m [^clojure.lang.Ratio left-neighbor-of-predecessor ^clojure.lang.Ratio predecessor] ^Boolean check-pair]
;; See Proposition 1.25 and Table 1.6 of the monograph. Call for instance:
;;    user>   (successor-of-pair-of-neighbors-in-Fm 6 [(/ 3 5) (/ 2 3)] true)
;; to get the result:
;;    3/4
;; Also call:
;;    user>   (successor-of-pair-of-neighbors-in-Fm 6 [(/ 3 5) (successor-in-Fm 6 (/ 3 5))] false)
;; to get the same result:
;;    3/4
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type left-neighbor-of-predecessor) clojure.lang.Ratio) (/ 1 (- 2))   ;"N/A: Type of left-neighbor-of-predecessor must be clojure.lang.Ratio"
    ;(not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 4))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< m 1) (/ 1 (- 5))   ;"N/A: Order m of the sequence should be > 0"
    (>= left-neighbor-of-predecessor predecessor) (/ 1 (- 6))   ;N/A: We should have leftNeighborOfPredecessor < predecessor"
    (or (<= predecessor (/ 0 1)) (>= predecessor (/ 1 1)))  (/ 1 (- 7))   ;"N/A: predecessor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
    (> (denominator predecessor) m)  (/ 1 (- 8))  ;"N/A: Denominator of the predecessor should not exceed the order m of the sequence"
    (< left-neighbor-of-predecessor (/ 0 1)) (/ 1 (- 9))   ;"N/A: leftNeighborOfPredecessor should be between (0 % 1) (included) and predecessor (excluded)"
    (> (denominator left-neighbor-of-predecessor) m) (/ 1 (- 10))   ;"N/A: Denominator of the leftNeighborOfPredecessor should not exceed the order m of the sequence"
    (or (not check-pair) (and check-pair (== predecessor (successor-in-Fm m left-neighbor-of-predecessor))))
    (let [farey-index (bigint (Math/floor (/ (+ m (denominator left-neighbor-of-predecessor))
                                             (denominator predecessor))))]
      (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                       (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor)))))
    :else (/ 1 (- 11))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey sequence"
    ))

(defn predecessor-of-pair-of-neighbors-in-Fml [m l [^clojure.lang.Ratio successor ^clojure.lang.Ratio right-neighbor-of-successor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fml
;; See Proposition 1.26 (ii) (a) and Table 1.6 of the monograph. Call for instance:
;;    user>   (predecessor-of-pair-of-neighbors-in-Fml 6 4 [(/ 4  5) (clojure.lang.Numbers/toRatio (/ 1 1))] true)
;; to get the result:
;;    3/4
;; Also call:
;;    user>   (predecessor-of-pair-of-neighbors-in-Fml 6 4 [(predecessor-in-Fml 6 4 (/ 1 1)) (/ 1 1)] false)
;; to get the same result:
;;    3/4
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    ;(not= (type right-neighbor-of-successor) clojure.lang.Ratio) (/ 1 (- 4))   ;"N/A: Type of right-neighbor-of-successor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 5))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< m 2) (/ 1 (- 6))   ;"N/A: Parameter m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 7))   ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
    (>= successor right-neighbor-of-successor) (/ 1 (- 8))   ;"N/A: We should have successor < right-neighbor-of-successor" 
    (or (<= successor (/ 0 1)) (> successor (/ 1 1))) (/ 1 (- 9))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)" 
    (> (denominator successor) m) (/ 1 (- 10))  ;"N/A: Denominator of the successor should not exceed the order m of the sequence" 
    (< l (numerator successor)) (/ 1 (- 11))  ;"N/A: Numerator of the successor should be between 1 (included) and l (included)"   
    (> right-neighbor-of-successor (/ 1 1)) (/ 1 (- 12))   ;"N/A: right-neighbor-of-successor should be between successor (excluded) and (1/1) (included)"
    (> (denominator right-neighbor-of-successor) m)  (/ 1 (- 13))   ;"N/A: Denominator of the rightNeighborOfSuccessor should not exceed the parameter m of the sequence"
    (< l (numerator right-neighbor-of-successor)) (/ 1 (- 14))   ;"N/A: Numerator of the rightNeighborOfSuccessor should be between 1 (included) and l (included)"
    (or (not check-pair) (and check-pair (== successor (predecessor-in-Fml m l right-neighbor-of-successor))))
    (if (>= (- (*' (numerator successor) m) (*' (denominator successor) l)) 1)
      (let [farey-index (bigint (Math/floor (/ (+ l (numerator right-neighbor-of-successor))
                                               (numerator successor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                         (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor)))))
      (let [farey-index (bigint (Math/floor (/ (+ m (denominator right-neighbor-of-successor))
                                               (denominator successor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                         (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor))))))
    :else (/ 1 (- 15))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
    ))

(defn successor-of-pair-of-neighbors-in-Fml [m l [^clojure.lang.Ratio left-neighbor-of-predecessor ^clojure.lang.Ratio predecessor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fml
;; See Proposition 1.26 (ii) (a) and Table 1.6 of the monograph. Call for instance:
;;    user>   (successor-of-pair-of-neighbors-in-Fml 6 4 [(/ 3 4) (/ 4 5)] true)
;; to get the result:
;;    
;; Also call:
;;    user>   (successor-of-pair-of-neighbors-in-Fml 6 4 [(/ 3 4) (successor-in-Fml 6 4 (/ 3 4))] false)
;; to get the same result:
;;    
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type left-neighbor-of-predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of left-neighbor-of-predecessor must be clojure.lang.Ratio"
    ;(not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 4))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 5))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< m 2) (/ 1 (- 6))   ;"N/A: Parameter m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 7))   ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
    (>= left-neighbor-of-predecessor predecessor) (/ 1 (- 8))   ;"N/A: We should have left-neighbor-of-predecessor < predecessor" 
    (or (<= predecessor (/ 0 1)) (>= predecessor (/ 1 1)))  (/ 1 (- 9))   ;"N/A: predecessor should be between (0/1) (excluded) and (1/1) (excluded)" 
    (> (denominator predecessor) m)  (/ 1 (- 10))  ;"N/A: Denominator of the predecessor should not exceed the order m of the sequence" 
    (< l (numerator predecessor)) (/ 1 (- 11))  ;"N/A: Numerator of the predecessor should be between 1 (included) and l (included)"   
    (< left-neighbor-of-predecessor (clojure.lang.Numbers/toRatio (/ 0 1))) (/ 1 (- 12))   ;"N/A: left-neighbor-of-predecessor should be between (0/1) (included) and predecessor (excluded)"
    (> (denominator left-neighbor-of-predecessor) m)  (/ 1 (- 13))   ;"N/A: Denominator of the left-neighbor-of-predecessor should not exceed the parameter m of the sequence"
    (< l (numerator left-neighbor-of-predecessor)) (/ 1 (- 14))   ;"N/A: Numerator of the left-neighbor-of-predecessor should be between 1 (included) and l (included)"
    (or (not check-pair) (and check-pair (== predecessor (successor-in-Fml m l left-neighbor-of-predecessor))))
    (if (>= (- (*' (denominator predecessor) l) (*' (numerator predecessor) m)) 1)
      (let [farey-index (bigint (Math/floor (/ (+ m (denominator left-neighbor-of-predecessor))
                                               (denominator predecessor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                         (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor)))))
      (let [farey-index (bigint (Math/floor (/ (+ l (numerator left-neighbor-of-predecessor))
                                               (numerator predecessor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                         (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor))))))
    :else (/ 1 (- 15))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
    ))

(defn predecessor-of-pair-of-neighbors-in-Gml [m l [^clojure.lang.Ratio successor ^clojure.lang.Ratio right-neighbor-of-successor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Gml
;; See Proposition 1.27 (ii) (a) and Table 1.6 of the monograph. Call for instance:
;;    user>   (predecessor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 2) (/ 3 5)] true)
;; to get the result:
;;    1/3
;; Also call:
;;    user>   (predecessor-of-pair-of-neighbors-in-Gml 6 4 [(predecessor-in-Gml 6 4 (/ 3 5)) (/ 3 5)] false)
;; to get the same result:
;;    1/3
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    ;(not= (type right-neighbor-of-successor) clojure.lang.Ratio) (/ 1 (- 4))   ;"N/A: Type of right-neighbor-of-successor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 5))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< m 2) (/ 1 (- 6))  ;"N/A: Parameter m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 7))   ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
    (>= successor right-neighbor-of-successor) (/ 1 (- 8))   ;"N/A: We should have successor < right-neighbor-of-successor" 
    (or (<= successor (/ 0 1)) (> successor (/ 1 1)))  (/ 1 (- 9))   ;"N/A: successor should be between (0/1) (excluded) and (1/1) (included)" 
    (> (denominator successor) m)  (/ 1 (- 10))  ;"N/A: Denominator of the successor should not exceed the order m of the sequence" 
    (> (- (+' l (denominator successor)) m) (numerator successor)) (/ 1 (- 11))  ;"N/A: The quantity (l + denominator - m) should not exceed the numerator of the successor"
    (> right-neighbor-of-successor (/ 1 1)) (/ 1 (- 12))   ;"N/A: right-neighbor-of-successor should be between successor (excluded) and (1/1) (included)"
    (> (denominator right-neighbor-of-successor) m)  (/ 1 (- 13))   ;"N/A: Denominator of the right-neighbor-of-successor should not exceed the parameter m of the sequence"
    (> (- (+' l (denominator right-neighbor-of-successor)) m) (numerator right-neighbor-of-successor)) (/ 1 (- 14))   ;"N/A: The quantity (l + denominator - m) should not exceed the numerator of the right-neighbor-of-successor"
    (or (not check-pair) (and check-pair (== successor (predecessor-in-Gml m l right-neighbor-of-successor))))
    (if (>= (- (*' (numerator successor) m) (*' (denominator successor) l)) 1)
      (let [farey-index (bigint (Math/floor (/ (+' m (denominator right-neighbor-of-successor))
                                               (denominator successor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                         (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor)))))
      (let [farey-index (bigint (Math/floor (/ (- (+' (- m l) (denominator right-neighbor-of-successor)) (numerator right-neighbor-of-successor))
                                               (- (denominator successor) (numerator successor)))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                         (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor))))))
    :else (/ 1 (- 15))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
    ))

(defn successor-of-pair-of-neighbors-in-Gml [m l [^clojure.lang.Ratio left-neighbor-of-predecessor ^clojure.lang.Ratio predecessor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Gml
;; See Proposition 1.27 (ii) (b) and Table 1.6 of the monograph. Call for instance:
;;    user>   (successor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 3) (/ 1 2)] true)
;; to get the result:
;;    3/5
;; Also call:
;;    user>   (successor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 3) (successor-in-Gml 6 4 (/ 1 3))] false)
;; to get the same result:
;;    3/5
  (cond
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type l) java.lang.Long) (not= (type l) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of l must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type left-neighbor-of-predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of left-neighbor-of-predecessor must be clojure.lang.Ratio"
    ;(not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 4))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 5))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< m 2) (/ 1 (- 6))  ;"N/A: Parameter m of the sequence should be > 1"
    (or (<= l 0) (>= l m)) (/ 1 (- 7))   ;"N/A: Parameter l should be between 0 (excluded) and m (excluded)"
    (>= left-neighbor-of-predecessor predecessor) (/ 1 (- 8))   ;"N/A: We should have left-neighbor-of-predecessor < predecessor"
    (or (<= predecessor (/ 0 1)) (> predecessor (/ 1 1)))  (/ 1 (- 9))   ;"N/A: predecessor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
    (> (denominator predecessor) m)  (/ 1 (- 10))  ;"N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
    (> (- (+' l (denominator predecessor)) m) (numerator predecessor)) (/ 1 (- 11))  ;"N/A: The quantity (l + denominator - m) should not exceed the numerator of the predecessor"
    (< left-neighbor-of-predecessor (/ 0 1)) (/ 1 (- 12))   ;"N/A: left-neighbor-of-predecessor should be between (0 % 1) (included) and predecessor (excluded)"
    (> (denominator left-neighbor-of-predecessor) m)  (/ 1 (- 13))   ;"N/A: Denominator of the left-neighbor-of-predecessor should not exceed the parameter m of the sequence"
    (> (- (+' l (denominator left-neighbor-of-predecessor)) m) (numerator left-neighbor-of-predecessor)) (/ 1 (- 14))   ;"N/A: The quantity (l + denominator - m) should not exceed the numerator of the left-neighbor-of-predecessor"
    (or (not check-pair) (and check-pair (== predecessor (successor-in-Gml m l left-neighbor-of-predecessor))))
    (if (>= (- (*' (denominator predecessor) l) (*' (numerator predecessor) m)) 1)
      (let [farey-index (bigint (Math/floor (/ (- (+' (- m l) (denominator left-neighbor-of-predecessor)) (numerator left-neighbor-of-predecessor))
                                               (- (denominator predecessor) (numerator predecessor)))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                         (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor)))))
      (let [farey-index (bigint (Math/floor (/ (+' m (denominator left-neighbor-of-predecessor))
                                               (denominator predecessor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                         (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor))))))
    :else (/ 1 (- 15))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
    ))

(defn predecessor-of-pair-of-neighbors-in-FBnm [n m [^clojure.lang.Ratio successor ^clojure.lang.Ratio right-neighbor-of-successor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence FBnm
;; See Proposition 1.28 (ii) (a) and Table 1.6 of the monograph. Call for instance:
;;    user>   (predecessor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 4 5) (clojure.lang.Numbers/toRatio (/ 1 1))] true)
;; to get the result:
;;    3/4
;; Also call:
;;    user>   (predecessor-of-pair-of-neighbors-in-FBnm 6 4 [(predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) (clojure.lang.Numbers/toRatio (/ 1 1))] false)
;; to get the same result:
;;    3/4
  (cond
    (and (not= (type n) java.lang.Long) (not= (type n) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of n must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type right-neighbor-of-successor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of right-neighbor-of-successor must be clojure.lang.Ratio"
    ;(not= (type successor) clojure.lang.Ratio) (/ 1 (- 4))   ;"N/A: Type of successor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 5))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< n 2) (/ 1 (- 6))   ;"N/A: Parameter n of the sequence should be > 1" 
    (or (< m 1) (>= m n)) (/ 1 (- 7))   ;"N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
    (>= successor right-neighbor-of-successor) (/ 1 (- 8))   ;"N/A: We should have successor < right-neighbor-of-successor" 
    (or (<= successor (/ 0 1)) (>= successor (/ 1 1)))  (/ 1 (- 9))   ;"N/A: successor should be between (0 % 1) (excluded) and (1 % 1) (excluded)"
    (> (denominator successor) n)  (/ 1 (- 10))  ;"N/A: Denominator of the successor should not exceed n"
    (or (> (- (+' m (denominator successor)) n) (numerator successor)) (> (numerator successor) m))  (/ 1 (- 11))  ;"N/A: Numerator of the successor should be between (m + denominator - n) (included) and m (included)"
    (> right-neighbor-of-successor (/ 1 1)) (/ 1 (- 12))   ;"N/A: rightNeighborOfSuccessor should be between successor (excluded) and (1 % 1) (included)"
    (> (denominator right-neighbor-of-successor) n)  (/ 1 (- 13))   ;"N/A: Denominator of the right-neighbor-of-successor should not exceed n"
    (or (> (- (+' m (denominator right-neighbor-of-successor)) n) (numerator right-neighbor-of-successor)) (> (numerator right-neighbor-of-successor) m)) (/ 1 (- 14))   ;"N/A: Numerator of the right-neighbor-of-successor should be between (m + denominator - n) (included) and m (included)"
    (or (not check-pair) (and check-pair (== successor (predecessor-in-FBnm n m right-neighbor-of-successor))))
    (if (>= (- (*' (numerator successor) n) (*' (denominator successor) m)) 1)
      (let [farey-index (bigint (Math/floor (/ (+' m (numerator right-neighbor-of-successor))
                                               (numerator successor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                         (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor)))))
      (let [farey-index (bigint (Math/floor (/ (- (+' (- n m) (denominator right-neighbor-of-successor)) (numerator right-neighbor-of-successor))
                                               (- (denominator successor) (numerator successor)))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator successor)) (numerator right-neighbor-of-successor))
                                         (- (*' farey-index (denominator successor)) (denominator right-neighbor-of-successor))))))
    :else (/ 1 (- 15))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
    ))

(defn successor-of-pair-of-neighbors-in-FBnm [n m [^clojure.lang.Ratio left-neighbor-of-predecessor ^clojure.lang.Ratio predecessor] ^Boolean check-pair]
;; If  (== checkPair true), then to check whether the input pair is indeed a pair of neighboring fractions in the sequence FBnm
;; See Proposition 1.28 (ii) (b) and Table 1.6 of the monograph. Call for instance:
;;    user>   (successor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 1 3) (/ 1 2)] true)
;; to get the result:
;;    3/5
;; Also call:
;;    user>   (successor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 1 3) (successor-in-FBnm 6 4 (/ 1 3))] false)
;; to get the same result:
;;    3/5
  (cond
    (and (not= (type n) java.lang.Long) (not= (type n) clojure.lang.BigInt)) (clojure.lang.Numbers/toRatio (/ 1 (- 1)))   ;"N/A: Type of n must be java.lang.Long or clojure.lang.BigInt"
    (and (not= (type m) java.lang.Long) (not= (type m) clojure.lang.BigInt)) (/ 1 (- 2))   ;"N/A: Type of m must be java.lang.Long or clojure.lang.BigInt"
    ;(not= (type left-neighbor-of-predecessor) clojure.lang.Ratio) (/ 1 (- 3))   ;"N/A: Type of left-neighbor-of-predecessor must be clojure.lang.Ratio"
    ;(not= (type predecessor) clojure.lang.Ratio) (/ 1 (- 4))   ;"N/A: Type of predecessor must be clojure.lang.Ratio"
    ;(not= (type check-pair) java.lang.Boolean) (/ 1 (- 5))   ;"N/A: Type of check-pair must be java.lang.Boolean"
    (< n 2) (/ 1 (- 6))  ;"N/A: Parameter n of the sequence should be > 1" 
    (or (< m 1) (>= m n)) (/ 1 (- 7))   ;"N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
    (>= left-neighbor-of-predecessor predecessor) (/ 1 (- 8))   ;"N/A: We should have left-neighbor-of-predecessor < predecessor" 
    (or (>= predecessor (/ 1 1)) (<= predecessor (/ 0 1))) (/ 1 (- 9))   ;"N/A: predecessor should be between (0/1) (excluded) and (1/1) (excluded)"
    (> (denominator predecessor) n) (/ 1 (- 10))  ;"N/A: Denominator of the predecessor should not exceed n"
    (or (> (- (+' m (denominator predecessor)) n) (numerator predecessor)) (> (numerator predecessor) m)) (/ 1 (- 11))  ;"N/A: Numerator of the left-neighbor-of-predecessor should be between (m + denominator - n) (included) and m (included)"
    (< left-neighbor-of-predecessor (/ 0 1)) (/ 1 (- 12))   ;"N/A: left-neighbor-of-predecessor should be between (0/1) (included) and predecessor (excluded)"
    (> (denominator left-neighbor-of-predecessor) n) (/ 1 (- 13))   ;"N/A: Denominator of the left-neighbor-of-predecessor should not exceed n"
    (or (> (- (+' m (denominator left-neighbor-of-predecessor)) n) (numerator left-neighbor-of-predecessor)) (> (numerator left-neighbor-of-predecessor) m)) (/ 1 (- 14))   ;"N/A: Numerator of the left-neighbor-of-predecessor should be between (m + denominator - n) (included) and m (included)"
    (or (not check-pair) (and check-pair (== predecessor (successor-in-FBnm n m left-neighbor-of-predecessor))))
    (if (>= (- (*' (denominator predecessor) m) (*' (numerator predecessor) n)) 1)
      (let [farey-index (bigint (Math/floor (/ (- (+' (- n m) (denominator left-neighbor-of-predecessor)) (numerator left-neighbor-of-predecessor))
                                               (- (denominator predecessor) (numerator predecessor)))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                         (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor)))))
      (let [farey-index (bigint (Math/floor (/ (+' m (numerator left-neighbor-of-predecessor))
                                               (numerator predecessor))))]
        (clojure.lang.Numbers/toRatio (/ (- (*' farey-index (numerator predecessor)) (numerator left-neighbor-of-predecessor))
                                         (- (*' farey-index (denominator predecessor)) (denominator left-neighbor-of-predecessor))))))
    :else (/ 1 (- 15))   ;"N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
    ))

(defn- predecessor-of-pair-of-neighbors-in-FB2mm [m [successor right-neighbor-of-successor] check-pair]
  (predecessor-of-pair-of-neighbors-in-FBnm (* 2 m) m [successor right-neighbor-of-successor] check-pair))

(defn- successor-of-pair-of-neighbors-in-FB2mm [m [left-neighbor-of-predecessor predecessor] check-pair]
  (predecessor-of-pair-of-neighbors-in-FBnm (* 2 m) m [left-neighbor-of-predecessor predecessor] check-pair))


