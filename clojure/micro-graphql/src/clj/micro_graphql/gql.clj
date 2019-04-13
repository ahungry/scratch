(ns micro-graphql.gql
  (:require
   [graphql-clj.executor :as executor]))

(def schema-str "type User {
    name: String
    age: Int
  }
  type QueryRoot {
    user: User
  }

  schema {
    query: QueryRoot
  }")

(defn resolver-fn [type-name field-name]
  (prn type-name "< Type")
  (prn field-name)
  (get-in {"QueryRoot" {"user" (fn [context parent args]
                                 {:name "test user name"
                                  :age 30})}}
          [type-name field-name]))

(def query-str "query {user {name age}}")

(executor/execute nil schema-str resolver-fn query-str)
