(ns counter-gui.core
  (:require
   [cljfx.api :as fx])
  (:import
   [javafx.scene.input KeyCode KeyEvent]
   [javafx.scene.paint Color]
   [javafx.scene.canvas Canvas])
  (:gen-class))

(def *state (atom {:clicked 0}))

(defn inc-or-make [n] (if n (inc n) 0))

(defn event-handler [event state]
  (case (:event/type event)
    ::stub (update-in state [:clicked] inc-or-make)
    state))

(def bws-prefix ::make-me-random)

(defn eh-bws-1 [event state]
  (case (:event/type event)
    ::clicked (update-in state [bws-prefix :clicked] inc-or-make)
    state))

(defn button-with-state [state]
  (let [{:keys [clicked]} (bws-prefix state)]
    {:fx/type :button
     :on-action {:event/type ::clicked}
     :text (str "Click me more! x " clicked)}))

(def event-handlers [event-handler eh-bws-1])

(defn run-event-handlers
  "If we have many event handler layers, we want to run each one in sequence.
  This could let us have `private` widgets that maintain a state."
  ([m]
   (prn "REH received only one arg? " m))
  ([state event]
   (let [f (reduce comp (map #(partial % event) event-handlers))]
     (f state))))

(defn root [{:keys [clicked] :as state}]
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
                   (button-with-state state)
                   ;; {:fx/type button-with-state}
                   ]}
           }

   })

(defn renderer []
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler #(swap! *state run-event-handlers %)}))

(defn main []
  (fx/mount-renderer *state (renderer)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
