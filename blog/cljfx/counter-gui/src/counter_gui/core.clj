(ns counter-gui.core
  (:require
   [cljfx.api :as fx])
  (:import
   [javafx.scene.input KeyCode KeyEvent]
   [javafx.scene.paint Color]
   [javafx.scene.canvas Canvas])
  (:gen-class))

(def *button-state-1 (atom {:clicked 0}))
(def *state (atom {:clicked 0}))

(defn event-handler [event]
  (case (:event/type event)
    ::stub (swap! *state update-in [:clicked] inc)
    (prn "Unhandled event")))

(defn button-with-state [{:keys [clicked]}]
  {:fx/type :button
   :on-action {:event/type ::stub}
   :text (str "Click me more! x" clicked)})

(defn root [{:keys [clicked]}]
  {:fx/type :stage
   :showing true
   :title "Counter"
   :width 300
   :height 300
   :scene {:fx/type :scene
           :stylesheets #{"styles.css"}
           :root {:fx/type :v-box
                  :children
                  [
                   {:fx/type :label :text (str "Root state is: " clicked)}
                   {:fx/type button-with-state}
                   ]}
           }

   })

(defn event-handler-bws [event]
  (case (:event/type event)
    ::stub
    (prn "EHBWS was clicked!")
    ;; (swap! *button-state-1 update-in [:clicked] inc)
    (prn "Unhandled event")))

(defn button-renderer []
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type button-with-state)
   :opts {:fx.opt/map-event-handler event-handler-bws}))

(defn renderer []
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-handler}))

(defn main []
  (fx/mount-renderer *button-state-1 (button-renderer))
  (fx/mount-renderer *state (renderer)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
