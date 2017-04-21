(ns advent11  (use clojure.test clojure.set) )

(def thulium-chip {:type :chip :element :thulium})
(def thulium-gen {:type :generator :element :thulium})
(def stronium-chip {:type :chip :element :stronium})
(def stronium-gen {:type :generator :element :stronium})
(def plutonium-chip {:type :chip :element :plutonium})
(def plutonium-gen {:type :generator :element :plutonium})
(def promethium-chip {:type :chip :element :promethium})
(def promethium-gen {:type :generator :element :promethium})
(def ruthenium-chip {:type :chip :element :ruthenium})
(def ruthenium-gen {:type :generator :element :ruthenium})

(def floors[#{thulium-chip thulium-gen plutonium-gen stronium-gen} #{plutonium-chip stronium-chip} #{promethium-gen promethium-chip ruthenium-chip ruthenium-gen} #{}])
(def start-state {:visited #{} :queue (sorted-map) :config {:floors floors :elevator 0 :steps 0  }} )
(def end-floor #{thulium-gen thulium-chip plutonium-chip plutonium-gen stronium-chip stronium-gen promethium-chip promethium-gen ruthenium-gen ruthenium-chip})


(defn is-generator? [x] (= :generator (:type x)))
(defn is-chip? [x] (= :chip (:type x)))

(defn is-floor-ok?
  "Checks if a floor config breaks any chips"
  {:test #(do
            (assert (= true (is-floor-ok? #{hydro-chip hydro-gen lith-gen})))
            (assert (= true (is-floor-ok? #{hydro-chip lith-chip})))
            (assert (= false (is-floor-ok? #{hydro-gen hydro-chip lith-chip})))
            (assert (= true (is-floor-ok? #{hydro-gen lith-gen})))
            (assert (= true (is-floor-ok? #{})))
            )}
  [floor] (let [chips (set(map :element (filter is-chip? floor))) generators (set(map :element (filter is-generator? floor)))]
                             (or (empty? generators) (every? (fn [x] (contains? generators x)) chips))))

(defn heuristic [floors]
  (apply + (for [n (range 4)] (let [ floor (get floors n)]
                       (+ (* (count floor)  (- 3 n)) )))))

(defn push-queue [queue prio obj]
  (def existing (get queue prio))
  (if existing (into queue {prio (conj existing obj)})
               (into queue {prio (conj [] obj) }))
  )

(defn peek-queue [queue]
  (first (second (first (seq queue)))))

(defn pop-queue [queue]
  (let [head (first queue) prio (first head) left-overs (rest(first(rest head))) ]
    (if (empty? left-overs) (dissoc queue prio) (into queue {prio (vec left-overs)}))))

(defn push-config-to-queue [queue config] (push-queue queue (+ (* (:steps config) 2) (heuristic (:floors config))) config))

(defn move [{elevator :elevator floors :floors steps :steps :as state} move-fn components]
  (let [new-elevator (move-fn elevator)]
 (if (or (> new-elevator 3 ) (< new-elevator 0))
   '()
   (let [old-floor (reduce disj (get floors elevator) components)
         new-floor (reduce conj (get floors new-elevator) components)]
     (if (and  (is-floor-ok? old-floor)
               (is-floor-ok? new-floor))
       (assoc state :floors (assoc floors elevator old-floor new-elevator new-floor) :elevator new-elevator :steps (inc steps) )
       '() )))))

(defn get-components [floor] (map vec (distinct(mapcat (fn [x] (map (fn [y] (set [x  y])) floor)) floor))))

(defn move-components[state components] (conj '() (move state inc components) (move state dec components )))

(defn visited-or-empty? [visited new-state]
  (or (empty? new-state) (contains? visited (dissoc new-state :steps))))

(defn add-visisted [visited config] (conj visited (dissoc config :steps)))
(defn solve-a [{visited :visited queue :queue config :config :as state} end-floor]
    (if (= (get (:floors config) 3 ) end-floor)
      (:steps config)
      (let [{:keys [floors elevator]} config ]
        (let [new-states (mapcat  (partial remove (partial visited-or-empty? visited))
                                  (map (partial move-components config) (get-components (get floors elevator))))
              new-queue (reduce push-config-to-queue queue new-states) ]
          (recur
            (assoc state :queue (pop-queue new-queue)
                             :visited (reduce add-visisted visited new-states)
                             :config (peek-queue new-queue)) end-floor)
      ))))

