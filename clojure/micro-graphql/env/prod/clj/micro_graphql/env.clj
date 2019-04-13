(ns micro-graphql.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[micro-graphql started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[micro-graphql has shut down successfully]=-"))
   :middleware identity})
