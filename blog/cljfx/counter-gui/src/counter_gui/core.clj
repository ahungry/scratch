(ns counter-gui.core
  (:require
   [cljfx.api :as fx])
  (:import
   [javafx.scene.input KeyCode KeyEvent]
   [javafx.scene.paint Color]
   [javafx.scene.canvas Canvas])
  (:gen-class))

(def *state (atom {:clicked 0}))

(defn event-handler [state event]
  (case (:event/type event)
    ::stub (update-in state [:clicked] inc)
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

(defn renderer []
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler #(swap! *state event-handler %)}))

(defn main []
  (fx/mount-renderer *state (renderer)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
