(ns ^:figwheel-always user-api-frontend.core)

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(def L (this-as ct (aget ct "L")))

(def santa-cruz {:lat 36.9719 :lon -122.0264})

(def map-center (array (:lat santa-cruz) (:lon santa-cruz)))

(def zoom 13)

(defn loadMap []
    (let [m (-> L (.map "map")
                (.setView map-center zoom))] m))

(defonce m (loadMap))

;(defonce app-state (atom {:text "Hello world!"}))

; mapbox id
(def mapbox-id "sampwing.300952ec")
(def mapbox-key "sk.eyJ1Ijoic2FtcHdpbmciLCJhIjoiY0RtWHFZVSJ9.JJSCPaQcohSy5ATZGWO3KQ")
(def map-tile-url (str "http://{s}.tiles.mapbox.com/v4/"
                       mapbox-id
                       "/{z}/{x}/{y}.png?access_token="
                       mapbox-key))
; set tile view
(.addTo (-> L (.tileLayer map-tile-url (clj->js {"maxZoom" 30}))) m)

(defn add-pin [{:keys [lat lon]}]
  (.addTo (-> L (.marker (array lat lon))) m))

(defn make-points []
  (loop [count 0
         points []]
    (let [lat-mod (* (Math/pow -1 (rand-int 2)) 1000.0)
          lon-mod (* (Math/pow -1 (rand-int 2)) 1000.0)
          lat (+ (/ (rand-int 10) lat-mod) (:lat santa-cruz))
          lon (+ (/ (rand-int 10) lon-mod) (:lon santa-cruz)),
          group (rand-int 3)
          point {:lat lat :lon lon :group group}]
      (add-pin point)
      (if (<= count 10)
        (recur (inc count) (conj points point))
        points))))

(def points (make-points))

(defn on-map-click [e]
  (let [popup (-> L (.popup))]
    (-> popup
        (.setLatLng (-> e (.-latlng)))
        (.setContent (str "You clicked at: " (-> e
                                                 (.-latlng)
                                                 (.toString))))
        (.openOn m))))

(-> m
    (.on "click" on-map-click))

(defn get-distance [{:keys [x y]}]
  ; get distance in meters between two points
  (let [x-pnt (-> L (.latLng (array (:lat x) (:lon x))))
        y-pnt (-> L (.latLng (array (:lat y) (:lon y))))]
    (-> x-pnt (.distanceTo y-pnt))))

(defn max-distance [{:keys [search-point points]}]
  ; get the max-distance between a point and the rest
  (loop [distance -1
         x (first points)
         point x
         points (rest points)]
    (if (nil? point)
      x
      (let [point-distance (get-distance {:x point :y search-point})]
        (if (> point-distance distance)
          (recur point-distance point (first points) (rest points))
          (recur distance x (first points) (rest points)))))))

(defn line-midpoint [{:keys [x y]}]
  ; find the midpoint between two points
  (let [lat (/ (+ (:lat x) (:lat y)) 2)
        lon (/ (+ (:lon x) (:lon y)) 2)
        r (/ (get-distance {:x x :y y}) 2)]
    {:center-lat lat :center-lon lon :radius r}))

(defn points-outside [{:keys [circle points]}]
  ; find points outside of the circle
  (filter #(let [left-side (Math/sqrt (+ (Math/pow (- (:lat %1) (:center-lat circle)) 2)
                                         (Math/pow (- (:lon %1) (:center-lon circle)) 2)))
                 right-side (:radius circle)]
                        (> left-side right-side)) points))

(defn ritter-algorithm [points]
  (let [y (first points)
        points (rest points)
        x (max-distance {:search-point y :points points})
        ; need to remove x from points here
        z (max-distance {:search-point x :points points})]
    (let [mid (line-midpoint {:x x :y z})]
      (println (str (:center-lat mid)  " : " (:center-lon mid) " : " (:radius mid)))
      (.addTo (-> L (.circle (array (:center-lat mid) (:center-lon mid)) (:radius mid))) m)
      (.addTo (-> L (.circle (array (:center-lat mid) (:center-lon mid)) (:radius mid))) m)
    (println (points-outside {:circle mid :points points})))
    ))

(ritter-algorithm points)

;(.addTo (-> L (.circle (array (:lat santa-cruz) (:lon santa-cruz)) 1300)) m)
