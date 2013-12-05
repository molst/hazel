(ns hazel.t-state
  (:use midje.sweet)
  (:use hazel.test)
  (:require [hazel.core :as db-util])
  (:require [hazel.state :as db])
  (:require [datomic.api :as d]))

;;Load the forms below to run all tests in the project.
#_(require 'midje.repl)
#_(midje.repl/load-facts)

(with-state-changes [(before :facts (require 'hazel.state :reload-all))]
  (fact "set storage type"
    (db/set-storage-type! :mem) => :mem))

(with-state-changes [(before :facts (reset-db! :mem))]
  (fact "storage type can only be set once"
    (db/set-storage-type! :mem) => (throws AssertionError)))

(with-state-changes [(before :facts (do (reset-db! :mem) (db/create! "test-db")))]
  (fact "created db is accessible"
    (= datomic.db.Db (type (db/db "test-db"))) => true))

(with-state-changes [(before :facts (init-db!))]
  (fact "dbfn-replacify-tx replaces existing values (that are not being added again)"
    (let [existing-entity (first (first (db-util/find (db/db "test-db") {:test/name "my-thing"})))]
      (db/dbfn-replacify-tx (db/db "test-db") (db-util/normalize-tx [[:db/add existing-entity :test/many-longs [1 4 5]]])) =>
      #(= (set %) #{[:db/retract existing-entity :test/many-longs 2]
                    [:db/add existing-entity :test/many-longs 1]
                    [:db/add existing-entity :test/many-longs 4]
                    [:db/add existing-entity :test/many-longs 5]}))))

(with-state-changes [(before :facts (init-db!))]
  (fact "basic execution of the database function dbfn-replacify-tx in the transactor works"
    (let [existing-entity (first (first (db-util/find (db/db "test-db") {:test/name "my-thing"})))
          db-after (:db-after @(d/transact (db/conn "test-db") (db/replacify-tx [[:db/add existing-entity :test/many-longs [6]]])))]
      (d/q `[:find ~'?val :where [~existing-entity :test/many-longs ~'?val]] db-after)) => #{[6]} >))

