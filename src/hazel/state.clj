(ns hazel.state "Database initialization and connection."
  (:require [datomic.api :as d])
  (:require [hazel.core :as db-util]))

(def db-function-tx "All db function transactions declared via 'defdbfn'." (atom []))

(defn add-db-functions! [db-conn] (d/transact db-conn @db-function-tx))

(defmacro defdbfn
  "Defines a function normally, and also adds a datomic db function transaction (that can be installed in a datomic db) to atom 'db-function-tx'."
  [name-symbol params code]
  `(let [~'fn-name-keyword (keyword '~name-symbol)]
     (if (some #{~'fn-name-keyword} (map :db/ident @db-function-tx))
       (throw (Exception. (str "Refusing to overwrite the already existing database function " ~'fn-name-keyword ".")))
       (do
         (defn ~name-symbol ~params ~code)
         (swap! db-function-tx conj (db-util/dbfn-transaction ~'fn-name-keyword (db-util/dbfn ~params ~code)))))))

(defdbfn dbfn-replacify-tx ;;Concatenates :db/retract facts for all :db/add facts for which the attributes have older values in the db.
  [db normalized-tx]
  (let [add-tx (filter #(= :db/add (first %)) normalized-tx) ;;take all add operations
        distinct-add-tx (distinct (map butlast normalized-tx))] ;;remove all duplicates due to multiple values
    (concat
     normalized-tx
     (for [trans distinct-add-tx :let [entid (nth trans 1)
                                       attr (nth trans 2)]
           already-existing-value (hazel.core/get-values db entid attr)
           :when (not (some #(= already-existing-value %) (map last add-tx)))] ;;do not retract values that are being added
       [:db/retract entid attr already-existing-value]))))

(defn replacify-tx "Makes application of 'dbfn-replacify-tx' a little easier." [tx] [[:dbfn-replacify-tx (hazel.core/normalize-tx tx)]])
