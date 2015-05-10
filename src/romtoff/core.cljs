(ns ^:figwheel-always romtoff.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [cljs.core.async :as async :refer [put! chan alts!]]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [fipp.edn :as fipp]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:tick 0
                          :entities {:dude {:ch (chan)
                                            :x 50
                                            :y 50
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
            (let [{:keys [type content]} (<! ch)]
              (case type
                :tween (om/transact! data :tweens #(conj % content))
                :update (om/transact! data #(merge % content))
                :transact (doseq [[key fn] content]
                            (om/transact! data key fn))))
            (recur))))
    om/IRender
    (render [_]
      (dom/g #js {:dangerouslySetInnerHTML #js {:__html (str "<image width=\"64\" height=\"64\" x=\"" x "\" y=\"" y "\" xlink:href=\"img/dude.png\" />")}
                  :transform (str "rotate(" (if rotation rotation 0) " 82 82)")
                  :onClick (fn [_]
                             (tell :dude {:type :tween
                                          :content [:y {:target (+ y 50)
                                                        :duration 30
                                                        :easing :bounce-out}]}))}))))

(om/root
  (fn [data owner]
    (reify
      om/IWillMount
      (will-mount [_]
        (js/setInterval #(om/transact! data :tick inc) 17))

      om/IRender
      (render [_]
        (doall (map (fn [[id entity]]
                      (doall (map (fn [[key {:keys [target duration easing progress initial] :as tween}]]
                                    (if-not progress
                                      (do
                                        (om/update! tween :progress 0)
                                        (om/update! tween :initial (get entity key)))
                                      (do
                                        (let [easing-fn (case easing :linear linear :cubic-out cubic-out :bounce-out bounce-out)]
                                          (om/update! entity key (easing-fn initial target progress duration)))
                                        (om/transact! tween :progress inc)
                                        (if (= duration progress) (om/transact! entity :tweens #(dissoc % key))))))
                                  (get entity :tweens))))
                    (get data :entities)))

        (dom/div nil
                 (dom/svg #js {:width 600
                               :height 400
                               :style #js {:float "left"
                                           :border "1px solid black"}
                               :onMouseMove (fn [e]
                                              (om/update! data [:mouse :prev] (get-in data [:mouse :current]))
                                              (om/update! data [:mouse :current] [(.-pageX e) (.-pageY e)])

                                              (when (get-in data [:mouse :down])
                                                (let [{:keys [current prev]} (get data :mouse)
                                                      dx (- (get current 0) (get prev 0))
                                                      dy (- (get current 1) (get prev 1))]
                                                  (tell :dude {:type :transact
                                                               :content {:x (partial + dx) :y (partial + dy)}}))))

                               :onMouseDown (fn [e]
                                              (om/update! data [:mouse :down] [(.-pageX e) (.-pageY e)]))

                               :onMouseUp #(om/update! data [:mouse :down] false)}

                          ;; (dom/circle #js {:cx 82
                          ;;                  :cy 82
                          ;;                  :r 26
                          ;;                  :fill "lightgreen"
                          ;;                  :onMouseOver #(println "over")
                          ;;                  :onMouseDown #(om/update! data [:moving] [(.-pageX %) (.-pageY %)])
                          ;;                  :onMouseUp #(om/update! data [:moving] false)})

                          (om/build dude (get-in data [:entities :dude])))

                 (dom/div #js {:style #js {:float "left"
                                           :width 400
                                           :height 400}}

                          (prn-str data))))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
