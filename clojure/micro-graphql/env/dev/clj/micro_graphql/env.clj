(ns micro-graphql.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [micro-graphql.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[micro-graphql started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[micro-graphql has shut down successfully]=-"))
   :middleware wrap-dev})
