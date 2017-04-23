(ns advent11
  (use clojure.test clojure.set)
  (:import (clojure.lang PersistentTreeMap)))

(defrecord Component [type element])
(defrecord Config [floors elevator steps])
(defrecord State [visited queue config])

(def thulium-chip (Component. :chip :thulium))
(def thulium-gen (Component. :generator :thulium))
(def stronium-chip (Component. :chip :stronium))
(def stronium-gen (Component. :generator :stronium))
(def plutonium-chip (Component. :chip :plutonium))
(def plutonium-gen (Component. :generator :plutonium))
(def promethium-chip (Component. :chip :promethium))
(def promethium-gen (Component. :generator :promethium))
(def ruthenium-chip (Component. :chip  :ruthenium))
(def ruthenium-gen (Component. :generator  :ruthenium))
(def elerium-chip (Component. :chip :elerium ))
(def elerium-gen (Component. :generator :elerium ))
(def dilithium-chip (Component. :chip :dilithium ))
(def dilithium-gen (Component. :generator :dilithium ))



(def floors-a[#{thulium-chip thulium-gen plutonium-gen stronium-gen} #{plutonium-chip stronium-chip} #{promethium-gen promethium-chip ruthenium-chip ruthenium-gen} #{}])
(def start-state-a (State.  #{}  (sorted-map)  (Config.  floors-a 0 0)))
(def end-floor-a #{thulium-gen thulium-chip plutonium-chip plutonium-gen stronium-chip stronium-gen promethium-chip promethium-gen ruthenium-gen ruthenium-chip})

(def floors-b[#{thulium-chip thulium-gen plutonium-gen stronium-gen elerium-gen elerium-chip dilithium-chip dilithium-gen} #{plutonium-chip stronium-chip} #{promethium-gen promethium-chip ruthenium-chip ruthenium-gen} #{}])
(def start-state-b (State.  #{}  (sorted-map)  (Config.  floors-b 0 0)))
(def end-floor-b #{thulium-gen thulium-chip plutonium-chip plutonium-gen stronium-chip stronium-gen promethium-chip promethium-gen ruthenium-gen ruthenium-chip elerium-chip elerium-gen dilithium-chip dilithium-gen})



(defn is-generator? [x] (= :generator (:type x)))
(defn is-chip? [x] (= :chip (:type x)))

(defn is-floor-ok?
  "Checks if a floor config breaks any chips"
  {:test #(do
            (assert (= true (is-floor-ok? #{thulium-chip thulium-gen stronium-gen})))
            (assert (= true (is-floor-ok? #{thulium-chip stronium-chip})))
            (assert (= false (is-floor-ok? #{thulium-gen thulium-chip stronium-chip})))
            (assert (= true (is-floor-ok? #{thulium-gen stronium-gen})))
            (assert (= true (is-floor-ok? #{}))))}

  [floor] (let [chips (set(map :element (filter is-chip? floor))) generators (set(map :element (filter is-generator? floor)))]
               (or (empty? generators) (every? (fn [x] (contains? generators x)) chips))))

(defn summarize-floor [floor] (let [chips (set(map :element (filter is-chip? floor)))
                                    generators (set(map :element (filter is-generator? floor)))
                                    elements (set (map :element floor))]
  (reduce (fn [sum e] (if (contains? generators e )
                        (if (contains? chips e)
                          (assoc sum :matched (inc (:matched sum)))
                          (assoc sum :gens (inc (:gens sum))))
                        (if (contains? chips e)
                          (assoc sum :gens (inc (:gens sum)))
                          sum))) {:matched 0 :chips 0 :gens 0} elements)))

(defn heuristic [floors]
  (apply + (for [n (range 4)] (let [ floor (get floors n)]
                               (+ (* (count floor)  (- 3 n)))))))


(defn push-queue [queue prio obj]
  (def existing (get queue prio))
  (if existing (into queue {prio (conj existing obj)})
               (into queue {prio (conj [] obj)})))


(defn peek-queue [queue]
  (first (second (first (seq queue)))))

(defn pop-queue [queue]
  (let [head (first queue) prio (first head) left-overs (rest(first(rest head)))]
    (if (empty? left-overs) (dissoc queue prio) (into queue {prio (vec left-overs)}))))

(defn push-config-to-queue [queue config] (push-queue queue (+ (* (:steps config) 2) (heuristic (:floors config))) config))

(defn move [{elevator :elevator floors :floors steps :steps} move-fn components]
  (let [new-elevator (move-fn elevator)]
   (if (or (> new-elevator 3 ) (< new-elevator 0))
     '()
     (let [old-floor (reduce disj (get floors elevator) components)
           new-floor (reduce conj (get floors new-elevator) components)]
       (if (and  (is-floor-ok? old-floor)
                 (is-floor-ok? new-floor))
         (Config. (assoc floors elevator old-floor new-elevator new-floor) new-elevator (inc steps))
         '())))))

(defn get-components [floor] (map vec (distinct(mapcat (fn [x] (map (fn [y] (set [x  y])) floor)) floor))))

(defn move-components[state components] (conj '() (move state inc components) (move state dec components)))

;(defn visited-or-empty? [visited new-state]
;  (or (empty? new-state) (contains? visited (dissoc new-state :steps))))

(defn visited-or-empty? [visited new-state]
  (or (empty? new-state) (contains? visited {:floors (map summarize-floor (:floors new-state)) :elevator (:elevator new-state)})))


(defn add-visisted [visited config] (conj visited {:floors (map summarize-floor (:floors config)) :elevator (:elevator config)}))

(defn solve [{visited :visited queue :queue config :config :as state} end-floor]
    (if (= (get (:floors config) 3 ) end-floor)
      (:steps config)
      (let [{:keys [floors elevator]} config]
        (let [new-states (mapcat  (partial remove (partial visited-or-empty? visited))
                                  (map (partial move-components config) (get-components (get floors elevator))))
              new-queue (reduce push-config-to-queue queue new-states)]
          (recur
             (State.  (reduce add-visisted visited new-states)
                      (pop-queue new-queue)
                      (peek-queue new-queue))
             end-floor)))))

(defn solve-a [] (println "Solution for A:" (solve start-state-a end-floor-a)))
(defn solve-b [] (println "Solution for B:" (solve start-state-b end-floor-b)))

