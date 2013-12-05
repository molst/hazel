(ns hazel.test "Test support."
  (:require [datomic.api :as d])
  (:require [hazel.core :as db-util])
  (:require [hazel.state :as db]))

(defn reset-db! [storage-type]
  (require 'hazel.state :reload-all)
  (db/set-storage-type! storage-type))

(def schema
  [{:db/id #db/id[:db.part/db]
    :db/ident :test/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/index true
    :db/doc "The name of this entity."
    :db.install/_attribute :db.part/db}
   {:db/id #db/id[:db.part/db]
    :db/ident :test/many-longs
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/many
    :db/unique :db.unique/identity
    :db/doc "Many longs."
    :db.install/_attribute :db.part/db}
   db-util/tags-schema])

(defn make-add-entity-transaction [ent] (db-util/normalize-tx [(merge {:db/id #db/id[:db.part/user]} ent)]))

(def test-entity {:test/name "my-thing" :test/many-longs [1 2]})

(defn init-db! []
  (reset-db! :mem)
  (db/create! "test-db")
  (d/transact (db/conn "test-db") schema)
  (d/transact (db/conn "test-db") (make-add-entity-transaction test-entity)))
