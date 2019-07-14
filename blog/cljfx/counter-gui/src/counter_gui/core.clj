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

(defn make-button-with-state
  "Wrapper to generate a stateful widget."
  [prefix]
  (let [handler (fn [event state]
                  "The event dispatching for received events."
                  [event state]
                  (if (= prefix (:prefix event))
                    (case (:event/type event)
                      ::clicked (update-in state [prefix :clicked] inc-or-make)
                      state)
                    state))
        view (fn [state]
               (let [{:keys [clicked]} (prefix state)]
                 {:fx/type :button
                  :on-action {:event/type ::clicked
                              :prefix prefix}
                  :text (str "Click me more! x " clicked prefix)}))]
    ;; Send the handler and view back up to the caller.
    {:handler handler
     :view view}))

(def bws-1 (make-button-with-state ::bws-1))
(def bws-2 (make-button-with-state ::bws-2))
(def event-handlers
  [event-handler
   (:handler bws-1)
   (:handler bws-2)])

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
                   ((:view bws-1) state)
                   ((:view bws-2) state)
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
