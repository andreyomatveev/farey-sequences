(ns fs.core
  (:gen-class)
  (:require [fs.fareysequences :refer [predecessor-in-Fm
                                       successor-in-Fm
                                       predecessor-in-Fml
                                       successor-in-Fml
                                       predecessor-in-Gml
                                       successor-in-Gml
                                       ;predecessor-in-FB2mm
                                       ;successor-in-FB2mm
                                       predecessor-in-FBnm
                                       successor-in-FBnm
                                       predecessor-of-pair-of-neighbors-in-Fm
                                       successor-of-pair-of-neighbors-in-Fm
                                       predecessor-of-pair-of-neighbors-in-Fml
                                       successor-of-pair-of-neighbors-in-Fml
                                       predecessor-of-pair-of-neighbors-in-Gml
                                       successor-of-pair-of-neighbors-in-Gml
                                       predecessor-of-pair-of-neighbors-in-FBnm
                                       successor-of-pair-of-neighbors-in-FBnm]]))

(defn -main []
  (println "(predecessor-in-Fm 6 (/ 2 3))      returns:     " (predecessor-in-Fm 6 (/ 2 3)))
  (println "(predecessor-in-Fm 6 (clojure.lang.Numbers/toRatio (/ 1 1)))       returns:     " (predecessor-in-Fm 6 (clojure.lang.Numbers/toRatio (/ 1 1))))
  (println "(predecessor-in-Fm 6 (clojure.lang.Numbers/toRatio 1))      returns:     " (predecessor-in-Fm 6 (clojure.lang.Numbers/toRatio 1)) "\n")
;;
  (println "(successor-in-Fm 6 (/ 1 3))      returns:     " (successor-in-Fm 6 (/ 1 3)))
  (println "(successor-in-Fm 6 (clojure.lang.Numbers/toRatio (/ 0 1)))      returns:     " (successor-in-Fm 6 (clojure.lang.Numbers/toRatio (/ 0 1))))
  (println "(successor-in-Fm 6 (clojure.lang.Numbers/toRatio 0))      returns:     " (successor-in-Fm 6 (clojure.lang.Numbers/toRatio 0)) "\n\n")
;;
;;
  (println "(predecessor-in-Fml 6 4 (/ 3 5))      returns:     " (predecessor-in-Fml 6 4 (/ 3 5)))
  (println "(predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1)))       returns:     " (predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))))
  (println "(predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio 1))      returns:     " (predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio 1)) "\n")
;;
  (println "(successor-in-Fml 6 4 (/ 4 5))      returns:     " (successor-in-Fml 6 4 (/ 4 5)))
  (println "(successor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 0 1)))      returns:     " (successor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 0 1))))
  (println "(successor-in-Fml 6 4 (clojure.lang.Numbers/toRatio 0))      returns:     " (successor-in-Fml 6 4 (clojure.lang.Numbers/toRatio 0)) "\n\n")
;;
;;
  (println "(predecessor-in-Gml 6 4 (/ 1 3))      returns:     " (predecessor-in-Gml 6 4 (/ 1 3)))
  (println "(predecessor-in-Gml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1)))      returns:     " (predecessor-in-Gml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))))
  (println "(predecessor-in-Gml 6 4 (clojure.lang.Numbers/toRatio 1))      returns:     " (predecessor-in-Gml 6 4 (clojure.lang.Numbers/toRatio 1)) "\n")
;;
  (println "(successor-in-Gml 6 4 (/ 1 3))      returns:     " (successor-in-Gml 6 4 (/ 1 3)))
  (println "(successor-in-Gml 6 4 (clojure.lang.Numbers/toRatio (/ 0 1)))      returns:     " (successor-in-Gml 6 4 (clojure.lang.Numbers/toRatio (/ 0 1))))
  (println "(successor-in-Gml 6 4 (clojure.lang.Numbers/toRatio 0))      returns:     " (successor-in-Gml 6 4 (clojure.lang.Numbers/toRatio 0)) "\n\n")
;;
;;
  ;(println "(predecessor-in-FB2mm 3 (/ 2 5))      returns:     " (predecessor-in-FB2mm 3 (/ 2 5)))
  ;(println "(predecessor-in-FB2mm 3 (clojure.lang.Numbers/toRatio (/ 1 1)))      returns:     " (predecessor-in-FB2mm 3 (clojure.lang.Numbers/toRatio (/ 1 1))))
  ;(println "(predecessor-in-FB2mm 3 (clojure.lang.Numbers/toRatio 1))      returns:     " (predecessor-in-FB2mm 3 (clojure.lang.Numbers/toRatio 1)) "\n")
;;
  ;(println "(successor-in-FB2mm 3 (/ 3 5))      returns:     " (successor-in-FB2mm 3 (/ 3 5)))
  ;(println "(successor-in-FB2mm 3 (clojure.lang.Numbers/toRatio (/ 0 1)))      returns:     " (successor-in-FB2mm 3 (clojure.lang.Numbers/toRatio (/ 0 1))))
  ;(println "(successor-in-FB2mm 3 (clojure.lang.Numbers/toRatio 0))      returns:     " (successor-in-FB2mm 3 (clojure.lang.Numbers/toRatio 0)) "\n\n")
;;
;;
  (println "(predecessor-in-FBnm 6 4 (/ 3 4))      returns:     " (predecessor-in-FBnm 6 4 (/ 3 4)))
  (println "(predecessor-in-FBnm 6 4  (clojure.lang.Numbers/toRatio (/ 1 1)))      returns:     " (predecessor-in-FBnm 6 4  (clojure.lang.Numbers/toRatio (/ 1 1))))
  (println "(predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio 1))      returns:     " (predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio 1)) "\n")
;;
  (println "(successor-in-FBnm 6 4 (/ 4 5))      returns:     " (successor-in-FBnm 6 4 (/ 4 5)))
  (println "(successor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 0 1)))      returns:     " (successor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 0 1))))
  (println "(successor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio 0))      returns:     " (successor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio 0)) "\n\n")
;;
;;
  (println "(predecessor-of-pair-of-neighbors-in-Fm 6 [(/ 1 3) (/ 2 5)] true)      returns:     " (predecessor-of-pair-of-neighbors-in-Fm 6 [(/ 1 3) (/ 2 5)] true))
  (println "(predecessor-of-pair-of-neighbors-in-Fm 6 [(predecessor-in-Fm 6 (/ 2 5)) (/ 2 5)] false)      returns:     " (predecessor-of-pair-of-neighbors-in-Fm 6 [(predecessor-in-Fm 6 (/ 2 5)) (/ 2 5)] false) "\n")
;;
  (println "(successor-of-pair-of-neighbors-in-Fm 6 [(/ 3 5) (/ 2 3) true)      returns:     " (successor-of-pair-of-neighbors-in-Fm 6 [(/ 3 5) (/ 2 3)] true))
  (println "(successor-of-pair-of-neighbors-in-Fm 6 [(/ 3 5) (successor-in-Fm 6 (/ 3 5))] false)      returns:     " (successor-of-pair-of-neighbors-in-Fm 6 [(/ 3 5) (successor-in-Fm 6 (/ 3 5))] false) "\n\n")
;;
;;
  (println "(predecessor-of-pair-of-neighbors-in-Fml 6 4 [(/ 4  5) (clojure.lang.Numbers/toRatio (/ 1 1))] true)      returns:     " (predecessor-of-pair-of-neighbors-in-Fml 6 4 [(/ 4  5) (clojure.lang.Numbers/toRatio (/ 1 1))] true))
  (println "(predecessor-of-pair-of-neighbors-in-Fml 6 4 [(predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) (clojure.lang.Numbers/toRatio (/ 1 1))] false)      returns:     " (predecessor-of-pair-of-neighbors-in-Fml 6 4 [(predecessor-in-Fml 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) (clojure.lang.Numbers/toRatio (/ 1 1))] false) "\n")
;;
  (println "(successor-of-pair-of-neighbors-in-Fml 6 4 [(/ 3 4) (/ 4 5)] true)      returns:     " (successor-of-pair-of-neighbors-in-Fml 6 4 [(/ 3 4) (/ 4 5)] true))
  (println "(successor-of-pair-of-neighbors-in-Fml 6 4 [(/ 3 4) (successor-in-Fml 6 4 (/ 3 4))] false)      returns:     " (successor-of-pair-of-neighbors-in-Fml 6 4 [(/ 3 4) (successor-in-Fml 6 4 (/ 3 4))] false) "\n\n")
;;
;;
  (println "(predecessor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 2) (/ 3 5)] true)      returns:     " (predecessor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 2) (/ 3 5)] true))
  (println "(predecessor-of-pair-of-neighbors-in-Gml 6 4 [(predecessor-in-Gml 6 4 (/ 3 5)) (/ 3 5)] false)      returns:     " (predecessor-of-pair-of-neighbors-in-Gml 6 4 [(predecessor-in-Gml 6 4 (/ 3 5)) (/ 3 5)] false) "\n")
;;
  (println "(successor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 3) (/ 1 2)] true)      returns:     " (successor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 3) (/ 1 2)] true))
  (println "(successor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 3) (successor-in-Gml 6 4 (/ 1 3))] false)      returns:     " (successor-of-pair-of-neighbors-in-Gml 6 4 [(/ 1 3) (successor-in-Gml 6 4 (/ 1 3))] false) "\n\n")
;;
;;  
  (println "(predecessor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 4 5) (clojure.lang.Numbers/toRatio (/ 1 1))] true)      returns:     " (predecessor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 4 5) (clojure.lang.Numbers/toRatio (/ 1 1))] true))
  (println "(predecessor-of-pair-of-neighbors-in-FBnm 6 4 [(predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) (clojure.lang.Numbers/toRatio (/ 1 1))] false)      returns:     " (predecessor-of-pair-of-neighbors-in-FBnm 6 4 [(predecessor-in-FBnm 6 4 (clojure.lang.Numbers/toRatio (/ 1 1))) (clojure.lang.Numbers/toRatio (/ 1 1))] false) "\n")
;;
  (println "(successor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 1 3) (/ 1 2)] true)      returns:     " (successor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 1 3) (/ 1 2)] true))
  (println "(successor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 1 3) (successor-in-FBnm 6 4 (/ 1 3))] false)      returns:     " (successor-of-pair-of-neighbors-in-FBnm 6 4 [(/ 1 3) (successor-in-FBnm 6 4 (/ 1 3))] false) "\n\n"))

