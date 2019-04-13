(ns micro-graphql.routes.home
  (:require
   [micro-graphql.layout :as layout]
   [clojure.java.io :as io]
   [micro-graphql.middleware :as middleware]
   [ring.util.http-response :as response]
   [micro-graphql.gql :as gql]))

(defn home-page [request]
  (layout/render request "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page [request]
  (layout/render request "about.html"))

(defn version [request]
  {:body {:version "0.0.1"}})

(defn gql [request]
  (prn request)
  {:body (gql/test-dice)})

(defn home-routes []
  [""
   {:middleware [ ;; middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/version" {:get version}]
   ["/gql" {:post gql}]
   ["/about" {:get about-page}]])
