(ns user
  (:require
    [micro-graphql.config :refer [env]]
    [clojure.spec.alpha :as s]
    [expound.alpha :as expound]
    [mount.core :as mount]
    [micro-graphql.core :refer [start-app]]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(defn start []
  (mount/start-without #'micro-graphql.core/repl-server))

(defn stop []
  (mount/stop-except #'micro-graphql.core/repl-server))

(defn restart []
  (stop)
  (start))


