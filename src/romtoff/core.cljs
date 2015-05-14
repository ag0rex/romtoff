(ns ^:figwheel-always romtoff.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [cljs.core.async :as async :refer [put! chan alts!]]
              [om.core :as om :include-macros true]
              [om-tools.dom :as dom :include-macros true]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:tick 0
                          :entities []}))

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

(defn by-id [entity-id]
  (first (filter #(= entity-id (:id %)) (:entities @app-state))))

(defn tell [entity-id message]
  (let [ch (:ch (by-id entity-id))]
    (put! ch message)))

(defn build-sprite [{:keys [id x y rotation ch animation sprite height width] :as data} owner event-handlers message-handlers]
  (reify
    om/IWillMount
    (will-mount [_]
      (go (loop []
            (let [messages (<! ch)
                  handlers (merge {:tween (fn [content] (om/transact! data :tweens #(merge % content)))
                                   :update (fn [content] (om/transact! data #(merge % content)))
                                   :transact (fn [content] (doseq [[key fn] content] (om/transact! data key fn)))}
                                  message-handlers)]
              (doseq [[type content] messages]
                (let [handler (handlers type)]
                  (if handler
                    (handler content)
                    (.warn js/console (str id ": Missing message handler for " type))))))
            (recur))))
    om/IRender
    (render [_]
      (let [img (if animation (:current animation) sprite)]
        (dom/g (merge {:dangerouslySetInnerHTML #js {:__html (str
                                                              "<image width=\"" width
                                                              "\" height=\"" height
                                                              "\" x=\"" x
                                                              "\" y=\"" y
                                                              "\" xlink:href=\"" img "\" />")}
                       :transform (str "rotate(" (if rotation rotation 0) " " (+ (/ width 2) x) " " (+ (/ height 2) y) ")")}
                      event-handlers))))))

(defn block [{:keys [x y rotation ch animation sprite height width] :as data} owner]
  (build-sprite data owner
                {:onClick (fn [_] (println x y))}
                {:boo (fn [_] (println "boo!!"))}))

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
      (dom/circle #js {:cx x :cy y :r 25
                       :onClick (fn [_] (put! game-chan :create))}))))

(defn from-default-entity [m]
  (merge {:ch (chan) :tweens {}} m))

(defn add-entity [data entity]
  (om/transact! data :entities #(conj % entity)))

(defmulti builder
  (fn [data owner] (:type data)))

(defmethod builder :sprite [data owner] (build-sprite data owner {} {}))

(defmethod builder :block [data owner] (block data owner))

(defmethod builder :falling-circle [data owner] (falling-circle data owner))

(om/root
  (fn [data owner]
    (reify
      om/IInitState
      (init-state [_]
        {:game-chan (chan)})

      om/IWillMount
      (will-mount [_]
        (js/setInterval #(om/transact! data :tick inc) 20)

        ;; (add-entity data [:dude (from-default-entity {:id :dude
        ;;                                               :type :sprite
        ;;                                               :x 50
        ;;                                               :y 50
        ;;                                               :height 64
        ;;                                               :width 64
        ;;                                               :animation {:frames ["img/block.jpg"
        ;;                                                                    "img/dude.png"
        ;;                                                                    "img/dude-nosed.png"]
        ;;                                                           :duration 20}})])

        (doseq [r (range 6)
                c (range 6)]
          (let [id (keyword (str "block-" r "-" c))]
            (add-entity data (from-default-entity {:id id
                                                   :type :sprite
                                                   :x (* r 62)
                                                   :y (* c 62)
                                                   :height 60
                                                   :width 60
                                                   :sprite "img/block.jpg"}))))

        (add-entity data (from-default-entity {:id :circle-1
                                               :type :falling-circle}))

        (let [game-chan (om/get-state owner :game-chan)]
          ;; Game channel.
          (go (loop []
                (let [msg (<! game-chan)]
                  (case msg
                    ;; :message action
                    (.warn js/console (str "Game: Missing message handler for " msg))))
                (recur)))))

      om/IDidMount
      (did-mount [_]
        (tell :block-0-0 {:boo true})
        (tell :circle-1 {:update {:x (rand 600)}
                         :tween {:y {:target 800
                                     :duration 30
                                     :easing :bounce-out
                                     }
                                 :x {:target (rand 600)
                                     :duration 60
                                     :easing :cubic-out
                                     :when-done :new-ball}}}))

      om/IRenderState
      (render-state [_ {:keys [game-chan]}]

        ;; Tween system.
        (doseq [entity (get data :entities)]
               (doseq [[key {:keys [target duration easing progress initial when-done] :as tween}] (get entity :tweens)]
                 (if-not progress
                   (do
                     (om/update! tween :progress 0)
                     (om/update! tween :initial (get entity key)))
                   (do
                     (let [easing-fn (case easing :linear linear :cubic-out cubic-out :bounce-out bounce-out)]
                       (om/update! entity key (easing-fn initial target progress duration)))
                     (om/transact! tween :progress inc)
                     (when (= duration progress)
                       (om/transact! entity :tweens #(dissoc % key))
                       (when when-done (put! game-chan when-done)))))))

        ;; Animation system.
        (doseq [entity (get data :entities)]
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

        (dom/div nil
                 (dom/svg #js {:width 600
                               :height 800
                               :style #js {:float "left"
                                           :border "1px solid lightgray"}
                               :onMouseMove (fn [e]
                                              (om/update! data [:mouse :prev] (get-in data [:mouse :current]))
                                              (om/update! data [:mouse :current] {:x (.-pageX e) :y (.-pageY e)})

                                              (when (get-in data [:mouse :down])
                                                (let [{:keys [current prev]} (get data :mouse)
                                                      dx (- (current :x) (prev :x))
                                                      dy (- (current :y) (prev :y))]
                                                  (do
                                                    ;; Drag.
                                                    ))))

                               :onMouseDown (fn [e]
                                              (om/update! data [:mouse :down] {:x (.-pageX e) :y (.-pageY e)}))

                               :onMouseUp (fn [e]
                                            (om/update! data [:mouse :down] false))}

                          (dom/rect #js {:x 0 :y 0
                                         :width 600 :height 800
                                         :style #js {:fill "rgb(250, 250, 200)"}})

                          (dom/g nil
                                 (map (fn [{:keys [type] :as entity}]
                                        (om/build builder entity {:init-state {:game-chan game-chan}}))
                                      (get data :entities))))

                 ;; Inspector.
                 (dom/div #js {:style #js {:float "left"
                                           :width 400
                                           :height 800}}
;;                          (prn-str data)

                          (let [data @data
                                no-chan-entities (reduce #(conj %1 (dissoc %2 :ch)) [] (:entities data))
                                no-chan-map (merge data {:entities no-chan-entities})]
                            (dom/pre nil
                                     (.stringify js/JSON (clj->js no-chan-map) nil 4))))))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
