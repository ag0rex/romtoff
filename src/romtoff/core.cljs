(ns ^:figwheel-always romtoff.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [cljs.core.async :as async :refer [put! chan alts!]]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:tick 0
                          :entities {:dude {:ch (chan)
                                            :x 50
                                            :y 50
                                            :animation {:frames ["img/dude.png"
                                                                 "img/dude-nosed.png"]
                                                        :duration 20}
                                            :tweens {}}
                                     :circle-1 {:ch (chan)
                                                :tweens {}}}}))

(defn linear [i t p d]
  (let [s (/ p d)]
    (+ i (* (- t i) s))))

(defn cubic-out [i t p d]
  (let [s (- (/ p d) 1)]
    (+ i (* (- t i) (+ 1 (* s s s))))))

(defn bounce-out [i t p d]
  (let [c (- t i)
        s (/ p d)]
    (if (< s (/ 1 2.75))
      (+ i (* c s s 7.5625))
      (if (< s (/ 2 2.75))
        (let [s (- s (/ 1.5 2.75))]
          (+ i (* c (+ 0.75 (* s s 7.5625)))))
        (if (< s (/ 2.5 2.75))
          (let [s (- s (/ 2.25 2.75))]
            (+ i (* c (+ 0.9375 (* s s 7.5625)))))
          (let [s (- s (/ 2.625 2.75))]
            (+ i (* c (+ 0.984375 (* s s 7.5625))))))))))

(defn tell [entity-id message]
  (let [ch (get-in @app-state [:entities entity-id :ch])]
    (put! ch message)))

(defn dude [{:keys [x y rotation ch] :as data} owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (go (loop []
            (let [messages (<! ch)]
              (doseq [[type content] messages]
                     (case type
                       :tween (om/transact! data :tweens #(merge % content))
                       :update (om/transact! data #(merge % content))
                       :transact (doseq [[key fn] content]
                                   (om/transact! data key fn)))))
            (recur))))
    om/IRender
    (render [_]
      (dom/g #js {:dangerouslySetInnerHTML #js {:__html (str "<image width=\"64\" height=\"64\" x=\"" x "\" y=\"" y "\" xlink:href=\"" (get-in data [:animation :current]) "\" />")}
                  :transform (str "rotate(" (if rotation rotation 0) " " (+ 32 x) " " (+ 32 y) ")")
                  :onClick (fn [_]
                             (tell :dude {:tween {:rotation {:target (+ rotation 360)
                                                             :duration 300
                                                             :easing :cubic-out}
                                                  :y {:target (rand 400)
                                                      :duration 30
                                                      :easing :bounce-out}
                                                  :x {:target (rand 400)
                                                      :duration 60
                                                      :easing :cubic-out}}}))}))))

(defn falling-circle [{:keys [ch x y] :as data} owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (go (loop []
            (let [messages (<! ch)]
              (doseq [[type content] messages]
                (case type
                  :tween (om/transact! data :tweens #(merge % content))
                  :update (om/transact! data #(merge % content))
                  :transact (doseq [[key fn] content]
                              (om/transact! data key fn)))))
            (recur))))
    om/IRenderState
    (render-state [_ {:keys [game-chan]}]
      (when (not (seq (:tweens data)))
        (put! game-chan :falling-over))

      (dom/circle #js {:cx x :cy y :r 25}))))

(om/root
  (fn [data owner]
    (reify
      om/IInitState
      (init-state [_]
        {:game-chan (chan)})

      om/IWillMount
      (will-mount [_]
        (js/setInterval #(om/transact! data :tick inc) 20)

        (let [game-chan (om/get-state owner :game-chan)]
          ;; Game channel.
          (go (loop []
                (let [msg (<! game-chan)]
                  (case msg
                    :falling-over (if (get @data :falling) (om/update! data :falling msg))))
                (recur)))))

      om/IRenderState
      (render-state [_ {:keys [game-chan]}]

        ;; Tween system.
        (doseq [[id entity] (get data :entities)]
               (doseq [[key {:keys [target duration easing progress initial] :as tween}] (get entity :tweens)]
                 (if-not progress
                   (do
                     (om/update! tween :progress 0)
                     (om/update! tween :initial (get entity key)))
                   (do
                     (let [easing-fn (case easing :linear linear :cubic-out cubic-out :bounce-out bounce-out)]
                       (om/update! entity key (easing-fn initial target progress duration)))
                     (om/transact! tween :progress inc)
                     (when (= duration progress) (om/transact! entity :tweens #(dissoc % key)))))))

        ;; Animation system.
        (doseq [[id entity] (get data :entities)]
          (when-let [{:keys [frames duration progress current] :as animation} (:animation entity)]
            (if-not progress
              (do
                (om/update! animation :progress 0)
                (om/update! animation :current (first frames)))
              (do
                (om/transact! animation :progress inc)
                (when (= duration progress)
                  (om/update! animation :progress 0)
                  (let [current-index (.indexOf (to-array frames) current)
                        next-index (if (= (dec (count frames)) current-index) 0 (inc current-index))]
                    (om/update! animation :current (get frames next-index))))))))

        (when-not (:falling data)
          (tell :circle-1 {:tween {:y {:target 400
                                       :duration 30
                                       :easing :bounce-out}
                                   :x {:target (rand 400)
                                       :duration 60
                                       :easing :cubic-out}}})
          (om/update! data :falling true))

        (dom/div nil
                 (dom/svg #js {:width 600
                               :height 400
                               :style #js {:float "left"
                                           :border "1px solid lightgray"}
                               :onMouseMove (fn [e]
                                              (om/update! data [:mouse :prev] (get-in data [:mouse :current]))
                                              (om/update! data [:mouse :current] {:x (.-pageX e) :y (.-pageY e)})

                                              (when (get-in data [:mouse :down])
                                                (let [{:keys [current prev]} (get data :mouse)
                                                      dx (- (current :x) (prev :x))
                                                      dy (- (current :y) (prev :y))]
                                                  (tell :dude {:transact {:x (partial + dx) :y (partial + dy)}}))))

                               :onMouseDown (fn [e]
                                              (om/update! data [:mouse :down] {:x (.-pageX e) :y (.-pageY e)}))

                               :onMouseUp (fn [e]
                                            (om/update! data [:mouse :down] false))}

                          (dom/rect #js {:x 0 :y 0
                                         :width 600 :height 400
                                         :style #js {:fill "rgb(250, 250, 200)"}})

                          ;; (om/build falling-circle (get-in data [:entities :circle-1]) {:init-state {:game-chan game-chan}})

                          (om/build dude (get-in data [:entities :dude]))
                          )

                 ;; Inspector.
                 (dom/div #js {:style #js {:float "left"
                                           :width 400
                                           :height 400}}
;;                          (prn-str data)

                          (let [data @data
                                no-chan-map (reduce #(update-in %1 [:entities %2] dissoc :ch) data (keys (:entities data)))]
                            (prn-str no-chan-map)
                            (dom/pre nil
                                     (.stringify js/JSON (clj->js no-chan-map) nil 4))))))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
